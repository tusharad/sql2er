create table department (dep_id serial primary key, dep_name varchar(30), createAt timestamptz default now());
create table employee ( employee_id serial primary key, employee_name varchar(30), employee_age int, 
    dep_id int references department (dep_id) on delete cascade, createAt timestamptz default now());
create table tasks (task_id int, task_name text);
