-- .read ./sql/query.sql
select distinct f.repr from (select id from tags where name = 'r:int') t join tag_to_function f2t on f2t.tag_id = t.id join functions f on f.id = f2t.function_id; 