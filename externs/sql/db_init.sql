PRAGMA foreign_keys = ON;

create table functions(id integer primary key, repr varchar(500));
create table tags(id integer primary key, name varchar(500));

create table tag_to_function(
    id integer primary key,
    tag_id integer,
    function_id integer, 
    -- constraints
    foreign key (tag_id) references tags(id),
    foreign key (function_id) references functions(id)
);

insert into functions (id, repr) values (1, 'main:int(int,char**)');
insert into functions (id, repr) values (2, 'add:int(int,int)'), (3, 'mul:int(int,int)');
insert into functions (id, repr) values (4, 'grid:int(int, int, char**)');


insert into tags (id, name) values (1, 'r:int'), (2, 'p:01:char**'), (3, 'p:02:int'), (4, 'p:01:int');

insert into tag_to_function (tag_id, function_id) values (1, 1), (1, 2), (1, 3); -- return int
insert into tag_to_function (tag_id, function_id) values (3, 2), (3, 3), (3, 4); -- take at least two int
insert into tag_to_function (tag_id, function_id) values (4, 2), (4, 3), (4, 1); -- take at least one int
insert into tag_to_function (tag_id, function_id) values (3, 1), (2, 4); -- take at least one char**^
