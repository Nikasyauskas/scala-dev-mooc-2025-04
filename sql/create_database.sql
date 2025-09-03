--setup database
drop database if exists demo;
create database demo;
\c demo;

create table public.result (
id BIGINT NOT NULL,
calculated_value DOUBLE PRECISION NOT NULL,
write_side_offset BIGINT NOT NULL,
PRIMARY KEY(id)
)

INSERT INTO public.result VALUES (1,0,1);