//MARBTEST JOB CLASS=A,MSGCLASS=X,NOTIFY=KWOYO01,REGION=0M,             JOB06168
// USER=KWOYO01
//STEP1    EXEC PGM=ISFAFD
//ISFOUT   DD SYSOUT=*
//ISFIN    DD *
  /F CICS00A1,MB01 UPD RED 8 8
/*
