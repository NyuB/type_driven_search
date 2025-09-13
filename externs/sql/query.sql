-- .read ./sql/query.sql
select f.name from tag_to_function
    join functions f on f.id = function_id
    join tags t on t.id = tag_id
    where t.name like 'p:02:int'
    
intersect

select f.name from tag_to_function
    join functions f on f.id = function_id
    join tags t on t.id = tag_id
    where t.name like 'p:01:char**'
;
