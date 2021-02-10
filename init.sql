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
  select 1, 'foo\'', '\'; DROP TABLE students; --\'' from dual
  where not exists (select * from bobby);
