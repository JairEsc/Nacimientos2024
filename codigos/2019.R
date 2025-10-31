#General 
nacimientos=data.table::fread("Inputs/2019/sinac2019DatosAbiertos.csv")
nacimientos=nacimientos |> 
  dplyr::filter(ENT_NAC==13)

conteo_p_loc=nacimientos |> 
  dplyr::group_by(ENT_NAC,MPO_NAC,LOC_NAC) |> 
  dplyr::summarise(num_nacimientos=dplyr::n())
conteo_p_clue=nacimientos |> 
  dplyr::group_by(ENT_NAC,MPO_NAC,LOC_NAC,CLUES) |> 
  dplyr::summarise(num_nacimientos=dplyr::n())


#source("../../Reutilizables/Postgres_BUIG/conexion_buig.R")

##clues_2025=st_read(buig,"13salud")
localidades_2024=st_read(buig,"localidades")
localidades_rurales=st_read(buig,"localidad_puntual_rural")

localidades_urb_y_rur=
  rbind(localidades_2024 |> dplyr::select(cvegeo,nomgeo,ambito,the_geom),
        localidades_rurales |> 
          dplyr::select(cvegeo,nomgeo,the_geom) |> 
          dplyr::mutate(ambito='Rural') |> 
          dplyr::mutate(cvegeo=substr(cvegeo,1,9)) |> 
          dplyr::filter(!cvegeo%in%localidades_2024$cvegeo))


conteo_p_loc_c_g=conteo_p_loc |> 
  dplyr::mutate(cvegeo=paste0(sprintf("%02d",ENT_NAC),sprintf("%03d",MPO_NAC),sprintf("%04d",LOC_NAC) ))|> 
  merge(localidades_urb_y_rur |> dplyr::select(cvegeo,nomgeo,ambito,the_geom),
        by='cvegeo',all.x=T)

conteo_p_loc_c_g=conteo_p_loc_c_g |> 
  dplyr::mutate(localidad_c_geometria=ifelse(!st_is_empty(the_geom),"Sí","No")) |> 
  #st_drop_geometry() |> 
  #dplyr::select(-the_geom) |> 
  dplyr::arrange(dplyr::desc(num_nacimientos))


conteo_p_loc_c_g=conteo_p_loc_c_g |> 
  dplyr::rename(ENTIDAD=ENT_NAC,
                MUNICIPIO=MPO_NAC,
                LOCALIDAD=LOC_NAC)

conteo_p_loc_c_g|> write.csv("Outputs/2019/conteo_por_localidad.csv",fileEncoding = "utf-8",row.names = F)


# conteo_p_loc_c_g=conteo_p_loc_c_g |> 
#   dplyr::filter(!st_is_empty(the_geom))
conteo_p_loc_c_g |> st_write("Outputs/2019/conteo_por_localidad.geojson",driver="GeoJSON")


#############Por clues
info_que_traia_de_clues=st_read(buig,"13salud")
conteo_p_clue$CLUES |> sapply(\(z){z%in%info_que_traia_de_clues$CLUES},simplify = T,USE.NAMES = F) |> sum()
info_que_traia_de_clues=info_que_traia_de_clues |> dplyr::select(clues,nom_mun,nombre,geom) |> 
  dplyr::rename(CLUES=clues)

conteo_p_clue_c_g=conteo_p_clue |> 
  merge(y = info_que_traia_de_clues,by='CLUES' ,all.x=T)

conteo_p_clue_c_g |>
  dplyr::arrange(dplyr::desc(num_nacimientos)) |> 
  dplyr::mutate(clues_c_geometria=ifelse(!st_is_empty(geom),"Sí","No")) |> 
  write.csv("Outputs/2019/conteo_por_CLUES.csv",fileEncoding = "UTF-8",row.names = F)

conteo_p_clue_c_g=conteo_p_clue_c_g |> 
  dplyr::filter(!st_is_empty(geom))

conteo_p_clue_c_g_sf=conteo_p_clue_c_g |> 
  st_as_sf(coords = c('LONGITUD','LATITUD'))
conteo_p_clue_c_g_sf=conteo_p_clue_c_g_sf |> 
  dplyr::arrange(dplyr::desc(num_nacimientos)) |> 
  st_set_crs(st_crs("EPSG:4326"))

library(leaflet)
leaflet() |> addTiles() |> addCircleMarkers(data=conteo_p_clue_c_g_sf)


conteo_p_clue_c_g |>dplyr::arrange(dplyr::desc(num_nacimientos)) |>  st_write("Outputs/2019/conteo_por_CLUES.geojson")
