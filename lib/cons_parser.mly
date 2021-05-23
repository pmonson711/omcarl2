%%

%public cons_exp:
  | ids= cons_id_list; COLON; srt= sort_exp        { `IdList (ids, Some srt) }
  | ids= cons_id_list; COLON;                      { `IdList (ids, None) }

cons_id_list:
  | lst= separated_nonempty_list(COMMA, ID)        { lst }
