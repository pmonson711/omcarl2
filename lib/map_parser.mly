%%

%public map_exp:
  | ids= map_id_list; COLON; srt= sort_exp         { `IdsDecl (ids, srt) }

map_id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }
