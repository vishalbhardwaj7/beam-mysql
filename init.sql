create database if not exists test;

use test;

create table if not exists nullable (
  id bigint not null primary key auto_increment,
  data varchar(255)
);

insert into nullable (data)
  select null from dual
  where not exists (select * from nullable);
