## DEMO POINTBLANK -------------------------------------------------------------
# Slides: https://joscani.github.io/pointblank_slides
# Autor: jlcr
# Fecha: 202306


library(tidyverse)
library(sparklyr)
library(pointblank)


## PATH DATOS ----

### En mi pc ----
# nyc_dir <-  "/media/hd1/nyc-taxi/"
# fs::dir_tree(nyc_dir)
# 
# (fn_1 <- str_glue("{nyc_dir}year=2020/month=1/part-0.parquet") )
# 
# fs::file_size(fn_1)

### en pc de Carlos 

nyc_dir <-  here::here("data/")
fs::dir_tree(nyc_dir)

(fn_1 <- str_glue("{nyc_dir}year=2020/month=1/part-0.parquet") )

fs::file_size(fn_1)

## LEER DATOS ----

# cambiar ruta si se utiliza mac de Carlos

# ruta_spark <- "~/spark/spark-3.3.2-bin-hadoop2/"

sc <- spark_connect(master = "local")
                    # spark_home = ruta_spark)

nyc_tabla <- sc  |>  
    spark_read_parquet(str_glue("{nyc_dir}year=2020/month=1/part-0.parquet"))

nyc_tabla


## REGLAS VALIDACIÓN----

agent <- 
    create_agent(
        tbl = nyc_tabla,
        tbl_name = "big_tabla_nyc"
    )  |>
    col_is_posix(vars(pickup_datetime))  |>
    col_vals_gt(vars(passenger_count), value = 0)  |>
    col_vals_gt(vars(trip_distance), value = 0)

agent

agent %>% 
    interrogate()

agent_int <- agent %>% 
    interrogate()

# por defecto 5000 filas
agent_int %>% 
    get_data_extracts( i = 2)

agent_int <- agent %>% 
    interrogate(get_first_n = 10000)

agent_int %>% 
    get_data_extracts( i = 2)


## SINTAXIS TIDYSELECT ----

# también sirve pasar strings 
agent2 <- 
    create_agent(
        tbl = nyc_tabla,
        tbl_name = "big_tabla_nyc"
    )  |>
    col_is_posix("pickup_datetime")  |>
    col_vals_gt(tip_amount:total_amount, value = 0)  |>
    col_vals_gt(c(pickup_location_id, dropoff_location_id), value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE))

# preguntar . 
# El color verde se atenúa cuando hay fallos
agent2  |>
    interrogate()


## ACTION LEVELS ----

al <- action_levels(
    warn_at = 0.05,
    stop_at = 0.2
) 

agent2_with_al <- 
    create_agent(
        tbl = nyc_tabla,
        tbl_name = "big_tabla_nyc", 
        actions = al
    )  |>
    col_is_posix(pickup_datetime)  |>
    col_vals_gt(tip_amount:total_amount, value = 0)  |>
    col_vals_gt(c(pickup_location_id, dropoff_location_id), value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE))

# preguntar 
agent2_with_al  |>
    interrogate()

# action levels a nivel variable



agent2_with_al_ind <- 
    create_agent(
        tbl = nyc_tabla,
        tbl_name = "big_tabla_nyc"
    )  |>
    col_is_posix(pickup_datetime)  |>
    col_vals_gt(tip_amount:total_amount, value = 0, actions = warn_on_fail() )  |>
    col_vals_gt(c(pickup_location_id, dropoff_location_id), value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE))

# preguntar 
agent2_with_al_ind  |>
    interrogate()

agent2_with_al_ind <- 
    create_agent(
        tbl = nyc_tabla,
        tbl_name = "big_tabla_nyc"
    )  |>
    col_is_posix(pickup_datetime)  |>
    col_vals_gt(tip_amount:total_amount, value = 0, actions = warn_on_fail() )  |>
    col_vals_gt(c(pickup_location_id, dropoff_location_id), value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE), 
                     actions = stop_on_fail())

agent2_with_al_ind  |>
    interrogate()


## OTRA TABLA

nyc_tabla_202201 <-  sc  |>  
    spark_read_parquet(str_glue("{nyc_dir}year=2022/month=1/part-0.parquet")) 


nuevo_agente <-  agent2_with_al |> 
    set_tbl(nyc_tabla_202201, 
            tbl_name = "Validacion Enero 2022") 

# preguntar 
nuevo_agente  |>
    interrogate()


## GUARDAR Y USAR EL AGENTE POSTERIORMENTE----

x_write_disk(agent2_with_al, filename = "agente_nyc.rds")



spark_disconnect(sc)

# Usar agente

sc <- spark_connect(master = "local",
                    spark_home = ruta_spark)


agente_nyc <- readRDS("~/Rstudio_projects/pointblank_demo/agente_nyc.rds")
agente_nyc



nyc_tabla_nueva <- sc  |>  
    spark_read_parquet(str_glue("{nyc_dir}year=2022/month=2/part-0.parquet"))


agente_nyc  |> 
    set_tbl(nyc_tabla_nueva) |> 
    interrogate()


## AGENTE a yml ----

nyc_tabla <- sc  |>  
    spark_read_parquet(str_glue("{nyc_dir}year=2020/month=1/part-0.parquet"))

nyc_tabla

al <- action_levels(
    warn_at = 0.05,
    stop_at = 0.2
) 




agent_to_yml <- 
    create_agent(
        tbl = ~nyc_tabla, # referenciar con ~ o funcion leer tabla
        tbl_name = "agente_en_yml",
        actions = al
    )  |>
    col_is_posix(pickup_datetime)  |>
    col_vals_gt(tip_amount:total_amount, value = 0)  |>
    col_vals_gt(c(pickup_location_id, dropoff_location_id), value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE))

agent_to_yml

yaml_write(agent_to_yml)


nyc_tabla <- sc  |>  
    spark_read_parquet(str_glue("{nyc_dir}year=2022/month=2/part-0.parquet"))

nyc_tabla
# da error !! 

yaml_agent_interrogate(filename = "agent-agente_en_yml.yml")

agent_to_yml <- 
    create_agent(
        tbl = ~nyc_tabla, # referenciar con ~ o funcion leer tabla
        tbl_name = "agente_en_yml",
        actions = al
    )  |>
    col_is_posix(pickup_datetime)  |>
    col_vals_gt(total_amount, value = 0)  |>
    col_vals_gt(pickup_location_id, value = 0)|> 
    col_vals_between(trip_distance, left = 0, right = 200, inclusive = c(FALSE, TRUE))

agent_to_yml

yaml_write(agent_to_yml)


## Con tablas de bd etc.. 
# pointblank::tbl_source()
# pointblank::tbl_get()

