procedure test1 (inf: integer; sup: integer; sum : out integer) is
	i : integer;
begin
	sum := 0;
	i := inf;
	if inf > 1 then
		while i <= sup loop
			sum := sum + i;
			i := i + 1;
		end loop;
	end if;
end;
