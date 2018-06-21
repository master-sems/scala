procedure test1 (I: integer, J: out integer) is
begin 
	pragma pre (I >= 0);
	if I > 0 then
		j := 1;
	else
		J := 0;
	end if;
	pragma post (I = J);
end;
 