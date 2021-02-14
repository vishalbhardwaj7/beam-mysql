create database if not exists test;

use test;

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
