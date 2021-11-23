create database if not exists test;

use test;

create table if not exists alt_parser (
  id bigint not null primary key auto_increment,
  some_text varchar(255),
  some_int bigint,
  some_double double,
  data bigint
);

insert into alt_parser ( some_int, data)
  select  7, 42 from dual
  where not exists (select * from alt_parser);

create table if not exists nullable (
  id bigint not null primary key auto_increment,
  data varchar(255)
);

insert into nullable (data)
  select null from dual
  where not exists (select * from nullable);

create table if not exists via_json (
  id bigint not null primary key,
  from_bool varchar(255) not null,
  from_double varchar(255) not null,
  from_string varchar(255) not null,
  from_array varchar(255) not null,
  from_object varchar(255) not null
);

create table if not exists bobby (
  id bigint not null primary key,
  bad_text varchar(255) not null,
  bad_text2 text not null
);

insert into bobby (id, bad_text, bad_text2)
  select 1, 'foo\'', '\'; DROP TABLE students; --' from dual
  where not exists (select * from bobby);

create table if not exists unicode (
  id bigint not null primary key auto_increment,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists pk_ai (
  id bigint not null primary key auto_increment,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists pk_ai2 (
  id bigint not null primary key auto_increment,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists pk_no_ai (
  id bigint not null primary key,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists no_pk (
  id bigint not null,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists latin1 (
  id bigint not null primary key auto_increment,
  data varchar(255) not null collate latin1_general_cs
);

create table if not exists bad_schema (
  id bigint not null primary key,
  data bigint
);

insert into bad_schema (id, data)
  select 1, 2 from dual
  where not exists (select * from bad_schema);

create table if not exists bad_schema_big (
  id bigint not null primary key,
  field_1 bit(1) not null,
  field_2 varchar(255) not null,
  field_3 varchar(255),
  field_4 bigint
);

insert into bad_schema_big (id, field_1, field_2, field_3, field_4)
  select 1, 0, 'bar', NULL, 10 from dual
  where not exists (select * from bad_schema_big);

create table if not exists bad_schema_nullable (
  id bigint not null primary key,
  field_1 varchar(255),
  field_2 bigint,
  field_3 bit(1),
  field_4 int
);

insert into bad_schema_nullable (id, field_1, field_2, field_3, field_4)
  select 1, 'bar', 15, 0, 15 from dual
  where not exists (select * from bad_schema_nullable);

create table if not exists lenient (
  id bigint not null primary key,
  int8_varchar varchar(255) not null,
  int16_varchar varchar(255) not null,
  int32_varchar varchar(255) not null,
  int64_varchar varchar(255) not null,
  float_varchar varchar(255) not null,
  double_varchar varchar(255) not null,
  text_tinyint tinyint not null,
  text_smallint smallint not null,
  text_int int not null,
  text_bigint bigint not null,
  text_float float not null,
  int8_float float not null,
  int16_float float not null,
  int32_float float not null,
  int64_float float not null,
  text_double double not null,
  int8_double double not null,
  int16_double double not null,
  int32_double double not null,
  int64_double double not null,
  viajson_binary varbinary(255) not null
);

insert into lenient (id,
                     int8_varchar,
                     int16_varchar,
                     int32_varchar,
                     int64_varchar,
                     float_varchar,
                     double_varchar,
                     text_tinyint,
                     text_smallint,
                     text_int,
                     text_bigint,
                     text_float,
                     int8_float,
                     int16_float,
                     int32_float,
                     int64_float,
                     text_double,
                     int8_double,
                     int16_double,
                     int32_double,
                     int64_double,
                     viajson_binary)
  select 1,
         '10',
         '256',
         '65666',
         '5000000000',
         '10.05',
         '10.0500505005',
         10,
         256,
         65666,
         5000000000,
         10.05,
         10.05,
         10.05,
         10.05,
         10.05,
         10.0500505005,
         10.0500505005,
         10.0500505005,
         10.0500505005,
         10.0500505005,
         '[1,2,3,4]' from dual
  where not exists (select * from lenient);
