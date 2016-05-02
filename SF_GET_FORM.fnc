CREATE OR REPLACE FUNCTION SF_GET_FORM (
                              i_LoggedInUserJasperOrgId IN ORG_NODE_DIM.ORG_NODEID%TYPE
                             ,i_ProductId  IN PRODUCT.PRODUCTID%TYPE
                             ,i_GradeId  IN GRADE_DIM.GRADEID%TYPE
                             ,i_AdminId  IN CUSTOMER_INFO.CUSTOMERID%TYPE
                             ,i_SubtestIdSingleSelect IN SUBTEST_DIM.SUBTESTID%TYPE
                             ,i_SubtestIdMultiselect IN VARCHAR2
                             ,i_Start_Test_Date IN VARCHAR
                             ,i_End_Test_Date IN VARCHAR
                             ,i_type IN NUMBER )
RETURN PRS_COLL_PGT_GLOBAL_TEMP_OBJ
IS

  /*******************************************************************************
  * FUNCTION:  sf_get_form
  * PURPOSE:   To get all form for different subtest
  * CREATED:   TCS  04/OCT/2013
  * NOTE:
  *
  * MODIFIED :
  * DATE         AUTHOR     DESCRIPTION
  *-------------------------------------------------------------------------------
  *
  ************************************************************************************/

PRAGMA AUTONOMOUS_TRANSACTION;

  t_PRS_PGT_GLOBAL_TEMP_OBJ  PRS_PGT_GLOBAL_TEMP_OBJ;
  t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ PRS_COLL_PGT_GLOBAL_TEMP_OBJ := PRS_COLL_PGT_GLOBAL_TEMP_OBJ();
  v_Subtest_Id SUBTEST_DIM.SUBTESTID%TYPE;
  v_SubtestMulti_Id VARCHAR2(20000);
  v_Cust_Id CUSTOMER_INFO.CUSTOMERID%TYPE;
  v_ELA CONSTANT  VARCHAR2(4):='3';
  v_OverAllComp CONSTANT  VARCHAR2(4):='7';
   v_Date_Start VARCHAR2(40);
  v_Date_End VARCHAR2(40);

  CURSOR c_Get_Form_For_Obj (Subtest_Id SUBTEST_DIM.SUBTESTID%TYPE )
  IS
  SELECT DISTINCT SOM.FORMID,SOM.FORM_NAME
          FROM MV_SUB_OBJ_FORM_MAP SOM
          WHERE SOM.SUBTESTID =  Subtest_Id
            AND EXISTS (SELECT 1 FROM OBJECTIVE_SCORE_FACT  OFT,
                                      ORG_LSTNODE_LINK      OLNK,
                                      CUST_PRODUCT_LINK CUST
                        WHERE OLNK.ORG_NODEID = i_LoggedInUserJasperOrgId
                           AND OLNK.ORG_LSTNODEID = OFT.ORG_NODEID
                           AND OLNK.ADMINID= OFT.ADMINID
                          AND OFT.SUBTESTID = SOM.SUBTESTID
                          AND OFT.objectiveID = SOM.objectiveID
                          AND CUST.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
                        AND OFT.CUST_PROD_ID = CUST.CUST_PROD_ID
                        AND OFT.ADMINID = CUST.ADMINID
                        AND OFT.FORMID = SOM.FORMID
                       )
   UNION


    SELECT DISTINCT SOM.FORMID,SOM.FORM_NAME
            FROM MV_SUB_OBJ_FORM_MAP SOM
            WHERE SOM.SUBTESTID =  Subtest_Id
             AND  EXISTS (SELECT 1 FROM   EDU_CENTER_DETAILS EDTLS,
                                          STUDENT_BIO_DIM STD,
                                          OBJECTIVE_SCORE_FACT OFT  ,
                                          CUST_PRODUCT_LINK CUST
                           WHERE EDTLS.EDU_CENTERID = i_LoggedInUserJasperOrgId
                             AND STD.EDU_CENTERID = EDTLS.EDU_CENTERID
                             AND CUST.CUSTOMERID =  EDTLS.CUSTOMERID
                             AND STD.ORG_NODEID = OFT.ORG_NODEID
                             AND OFT.STUDENT_BIO_ID = STD.STUDENT_BIO_ID
                             AND STD.GRADEID =  DECODE(i_GradeId,
                                                  -99,
                                                  (SELECT GRADEID
                                                     FROM GRADE_DIM
                                                    WHERE GRADE_NAME = 'AD'
                                                      AND ROWNUM = 1),
                                                  NULL,
                                                  (SELECT GRADEID
                                                     FROM GRADE_DIM
                                                    WHERE GRADE_NAME = 'AD'
                                                      AND ROWNUM = 1),
                                                  i_GradeId)
                            -- AND STD.CUSTOMERID = v_Cust_Id -- changed by Debashis on 29/01/2014 For Performance
                             AND STD.CUSTOMERID = EDTLS.CUSTOMERID
                             AND STD.GRADEID = OFT.GRADEID
                             AND STD.ADMINID = OFT.ADMINID
                             AND OFT.SUBTESTID = SOM.SUBTESTID
                             AND OFT.LEVELID = SOM.LEVELID
                              AND CUST.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
                        AND OFT.CUST_PROD_ID = CUST.CUST_PROD_ID
                        AND OFT.ADMINID = CUST.ADMINID
                        AND OFT.objectiveID = SOM.objectiveID
                        AND OFT.FORMID = SOM.FORMID
                        AND ROWNUM =1)
               ORDER BY 1;




 CURSOR c_Get_Form_For_Sub (v_SubtestMulti_Id VARCHAR2)
  IS
   SELECT DISTINCT SOM.FORMID,SOM.FORM_NAME
              FROM MV_SUB_OBJ_FORM_MAP SOM
              WHERE SOM.SUBTESTID IN (SELECT TRIM( SUBSTR ( txt
                                                   , INSTR (txt, ',', 1, level ) + 1
                                                   , INSTR (txt, ',', 1, level+1
                                                   )
                                             - INSTR (txt, ',', 1, level) -1 ) ) AS u
                                      FROM ( SELECT ','||v_SubtestMulti_Id||',' AS txt
                                                FROM dual )
                                       CONNECT BY level <=
                                                    LENGTH(txt)-LENGTH(REPLACE(txt,',',''))-1  )
                AND EXISTS (SELECT 1 FROM SUBTEST_SCORE_FACT SCR,
                                          ORG_LSTNODE_LINK OLNK,
                                           --ORGUSER_MAPPING OUSR,
                                              CUST_PRODUCT_LINK CUST
                                 WHERE OLNK.ORG_NODEID = i_LoggedInUserJasperOrgId
                                   AND OLNK.ORG_LSTNODEID = SCR.ORG_NODEID
                                   AND OLNK.ADMINID= SCR.ADMINID
                                    /*UPPER(OUSR.USERNAME) = UPPER(i_LoggedInUserName)
                                   AND OUSR.LOWEST_NODEID = SCR.ORG_NODEID
                                   AND OUSR.ADMINID = SCR.ADMINID
                                   AND OUSR.CUSTOMERID = v_Cust_Id*/
                                   AND SCR.ASSESSMENTID = SOM.ASSESSMENTID
                                   AND SCR.SUBTESTID = SOM.SUBTESTID
                                   AND SCR.LEVELID = SOM.LEVELID
                                   AND SCR.FORMID = SOM.FORMID
                                    AND CUST.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
                                    AND SCR.CUST_PROD_ID = CUST.CUST_PROD_ID
                                    AND SCR.ADMINID = CUST.ADMINID)

          UNION
          SELECT DISTINCT SOM.FORMID,SOM.FORM_NAME
              FROM --FORM_DIM FRM,
                   MV_SUB_OBJ_FORM_MAP SOM
                   --LEVEL_MAP LMP
              WHERE SOM.SUBTESTID IN (SELECT TRIM( SUBSTR ( txt
                                                   , INSTR (txt, ',', 1, level ) + 1
                                                   , INSTR (txt, ',', 1, level+1
                                                   )
                                             - INSTR (txt, ',', 1, level) -1 ) ) AS u
                                      FROM ( SELECT ','||v_SubtestMulti_Id||',' AS txt
                                                FROM dual )
                                       CONNECT BY level <=
                                                    LENGTH(txt)-LENGTH(REPLACE(txt,',',''))-1  )
                 AND EXISTS (SELECT 1 FROM  SUBTEST_SCORE_FACT SCR,
                                            STUDENT_BIO_DIM STD,
                                            --GRADE_SELECTION_LOOKUP GSL ,
                                            --EDU_CENTER_USER_LINK ELINK,
                                            EDU_CENTER_DETAILS EDTLS,
                                            --USERS USR,
                                            CUST_PRODUCT_LINK CUST
                                            --ASSESSMENT_DIM ASES
                                   WHERE  EDTLS.EDU_CENTERID =  i_LoggedInUserJasperOrgId
                                     AND STD.CUSTOMERID = EDTLS.CUSTOMERID
                                     AND STD.EDU_CENTERID = EDTLS.EDU_CENTERID
                                     AND CUST.CUSTOMERID = EDTLS.CUSTOMERID
                                     AND STD.ORG_NODEID = SCR.ORG_NODEID
                                     AND STD.ADMINID = SCR.ADMINID
                                     AND STD.GRADEID =DECODE(i_GradeId,
                                                  -99,
                                                  (SELECT GRADEID
                                                     FROM GRADE_DIM
                                                    WHERE GRADE_NAME = 'AD'
                                                      AND ROWNUM = 1),
                                                  NULL,
                                                  (SELECT GRADEID
                                                     FROM GRADE_DIM
                                                    WHERE GRADE_NAME = 'AD'
                                                      AND ROWNUM = 1),
                                                  i_GradeId)
                                     AND STD.GRADEID = SCR.GRADEID
                                     AND STD.STUDENT_BIO_ID = SCR.STUDENT_BIO_ID
                                     AND STD.GENDERID = SCR.GENDERID
                                     AND SCR.LEVELID = SOM.LEVELID
                                     AND SCR.FORMID = SOM.FORMID
                                     AND SCR.ASSESSMENTID = SOM.ASSESSMENTID
                                     AND CUST.PRODUCTID = DECODE(i_ProductId,
                                                           -99,
                                                           (SELECT PRODUCTID
                                                              FROM PRODUCT
                                                             WHERE PRODUCT_SEQ = 1
                                                               AND ROWNUM = 1),
                                                           NULL,
                                                           (SELECT PRODUCTID
                                                              FROM PRODUCT
                                                             WHERE PRODUCT_SEQ = 1
                                                               AND ROWNUM = 1),
                                                           i_ProductId)
                                    AND SCR.CUST_PROD_ID = CUST.CUST_PROD_ID
                                    AND SCR.ADMINID = CUST.ADMINID
                                     AND ROWNUM =1)

           ORDER BY 1;



 CURSOR c_Get_Subtest_Obj_Login (Cust_Id CUSTOMER_INFO.CUSTOMERID%TYPE,p_Date_Start IN VARCHAR,p_Date_End IN VARCHAR)
  IS
  SELECT SUBTESTID,
         SUBTEST_NAME,
         SUBTEST_SEQ
  FROM
  (SELECT DISTINCT SOM.SUBTESTID, SOM.SUBTEST_NAME, SOM.SUBTEST_SEQ
    FROM MV_SUB_OBJ_FORM_MAP SOM
   WHERE SOM.SUBTEST_CODE NOT IN (v_ELA, v_OverAllComp)
     AND EXISTS
   (SELECT 1
            FROM ORG_LSTNODE_LINK      OLNK,
                 OBJECTIVE_SCORE_FACT OFT,
                 CUST_PRODUCT_LINK    CUST
           WHERE OLNK.ORG_NODEID = i_LoggedInUserJasperOrgId
             AND OLNK.ORG_LSTNODEID = OFT.ORG_NODEID
             AND OLNK.ADMINID= OFT.ADMINID
             AND OFT.SUBTESTID = SOM.SUBTESTID
             AND OFT.OBJECTIVEID = SOM.OBJECTIVEID
             AND OFT.FORMID = SOM.FORMID
             AND CUST.PRODUCTID = DECODE(i_ProductId,
                                         -99,
                                         (SELECT PRODUCTID
                                            FROM PRODUCT
                                           WHERE PRODUCT_SEQ = 1
                                             AND ROWNUM = 1),
                                         NULL,
                                         (SELECT PRODUCTID
                                            FROM PRODUCT
                                           WHERE PRODUCT_SEQ = 1
                                             AND ROWNUM = 1),
                                         i_ProductId)
             AND CUST.CUST_PROD_ID = OFT.CUST_PROD_ID
             AND CUST.ADMINID = OFT.ADMINID
                --AND OFT.GRADEID = i_GradeId
             AND OFT.LEVELID = SOM.LEVELID
             AND OFT.ASSESSMENTID = SOM.ASSESSMENTID
             /*AND OFT.SS > 0*/--commented as directed during QA 03/27/2015
             )

          UNION

           SELECT  DISTINCT SOM.SUBTESTID,
                   SOM.SUBTEST_NAME,
                   SOM.SUBTEST_SEQ
            FROM MV_SUB_OBJ_FORM_MAP SOM
            WHERE  SOM.SUBTEST_CODE NOT IN (v_ELA, v_OverAllComp)
            AND EXISTS (SELECT 1 FROM    STUDENT_BIO_DIM STD,
                                         OBJECTIVE_SCORE_FACT OFT ,
                                         CUST_PRODUCT_LINK CUST,
                                         EDU_CENTER_DETAILS EDTLS
                           WHERE EDTLS.EDU_CENTERID = i_LoggedInUserJasperOrgId
                             AND EDTLS.EDU_CENTERID = STD.EDU_CENTERID
                             AND EDTLS.CUSTOMERID = STD.CUSTOMERID
                             AND EDTLS.CUSTOMERID = CUST.CUSTOMERID
                             AND STD.ORG_NODEID = OFT.ORG_NODEID
                             AND OFT.STUDENT_BIO_ID = STD.STUDENT_BIO_ID
                             AND CUST.PRODUCTID = DECODE(i_ProductId,
                                                   -99,
                                                   (SELECT PRODUCTID
                                                      FROM PRODUCT
                                                     WHERE PRODUCT_SEQ = 1
                                                       AND ROWNUM = 1),
                                                   NULL,
                                                   (SELECT PRODUCTID
                                                      FROM PRODUCT
                                                     WHERE PRODUCT_SEQ = 1
                                                       AND ROWNUM = 1),
                                                   i_ProductId)
                             AND CUST.CUST_PROD_ID = OFT.CUST_PROD_ID
                             AND CUST.ADMINID = OFT.ADMINID
                             --AND STD.GRADEID = i_GradeId
                             AND STD.GRADEID = OFT.GRADEID
                             AND STD.ADMINID = OFT.ADMINID
                             AND OFT.SUBTESTID = SOM.SUBTESTID
                             AND OFT.LEVELID = SOM.LEVELID
                             AND OFT.ASSESSMENTID = SOM.ASSESSMENTID
                             AND OFT.objectiveID = SOM.objectiveID
                             AND OFT.FORMID = SOM.FORMID
                             /*AND OFT.SS > 0*/ --commented as directed during QA 03/27/2015
                             )
              ORDER BY 3) WHERE ROWNUM=1;

  CURSOR c_Get_Subtest_Sub_Login (Cust_Id CUSTOMER_INFO.CUSTOMERID%TYPE)
  IS
  SELECT LISTAGG(A.SUBTESTID, ',') WITHIN GROUP (ORDER BY A.SUBTEST_SEQ) AS SUBTEST_MULTI
  FROM
  (SELECT DISTINCT SOM.SUBTESTID, SOM.SUBTEST_NAME, SOM.SUBTEST_SEQ
          FROM MV_SUB_OBJ_FORM_MAP SOM,
               ORG_LSTNODE_LINK OLNK,
               ASSESSMENT_DIM ASES,
               GRADE_SELECTION_LOOKUP GSL
         WHERE OLNK.ORG_NODEID = i_LoggedInUserJasperOrgId
             AND OLNK.ORG_LSTNODEID = GSL.ORG_NODEID
             AND OLNK.ADMINID= GSL.ADMINID
             AND GSL.ASSESSMENTID = ASES.ASSESSMENTID
             AND GSL.GRADEID = DECODE(i_GradeId,
                                    -99,
                                    (SELECT GRADEID
                                       FROM GRADE_DIM
                                      WHERE GRADE_NAME = 'AD'
                                        AND ROWNUM = 1),
                                    NULL,
                                    (SELECT GRADEID
                                       FROM GRADE_DIM
                                      WHERE GRADE_NAME = 'AD'
                                        AND ROWNUM = 1),
                                    i_GradeId)
           AND ASES.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
           AND SOM.FORMID = GSL.FORMID
           AND SOM.ASSESSMENTID = GSL.ASSESSMENTID
           AND SOM.LEVELID = GSL.LEVELID

        UNION

        SELECT DISTINCT SOM.SUBTESTID, SOM.SUBTEST_NAME, SOM.SUBTEST_SEQ
          FROM MV_SUB_OBJ_FORM_MAP SOM,
               EDU_CENTER_DETAILS EDTLS
         WHERE  EDTLS.EDU_CENTERID =  i_LoggedInUserJasperOrgId
           AND EXISTS (SELECT 1
                  FROM SUBTEST_SCORE_FACT SCR, STUDENT_BIO_DIM STD,CUST_PRODUCT_LINK CUST
                 WHERE STD.ORG_NODEID = SCR.ORG_NODEID
                   AND STD.STUDENT_BIO_ID = SCR.STUDENT_BIO_ID
                   AND STD.EDU_CENTERID = EDTLS.EDU_CENTERID
                   AND STD.CUSTOMERID = EDTLS.CUSTOMERID
                   AND CUST.CUSTOMERID = EDTLS.CUSTOMERID
                   AND SCR.SUBTESTID = SOM.SUBTESTID
                   AND SCR.LEVELID = SOM.LEVELID
                   AND SCR.ASSESSMENTID = SOM.ASSESSMENTID
                   AND SCR.FORMID = SOM.FORMID
                    AND CUST.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
                    AND CUST.CUST_PROD_ID = SCR.CUST_PROD_ID
                    AND CUST.ADMINID = SCR.ADMINID
                   AND SCR.SS > 0)
         ORDER BY 3) A;

 CURSOR c_Get_Subtest_Hse_Login (Cust_Id CUSTOMER_INFO.CUSTOMERID%TYPE)
  IS
  SELECT LISTAGG(A.SUBTESTID, ',') WITHIN GROUP (ORDER BY A.SUBTEST_SEQ) AS SUBTEST_MULTI
  FROM
  ( SELECT DISTINCT SOM.SUBTESTID, SOM.SUBTEST_NAME, SOM.SUBTEST_SEQ
          FROM MV_SUB_OBJ_FORM_MAP SOM,
               ORG_LSTNODE_LINK OLNK,
               ASSESSMENT_DIM ASES,
               GRADE_SELECTION_LOOKUP GSL
         WHERE OLNK.ORG_NODEID = i_LoggedInUserJasperOrgId
             AND OLNK.ORG_LSTNODEID = GSL.ORG_NODEID
             AND OLNK.ADMINID= GSL.ADMINID
             AND GSL.ASSESSMENTID = ASES.ASSESSMENTID
             AND GSL.GRADEID = DECODE(i_GradeId,
                                    -99,
                                    (SELECT GRADEID
                                       FROM GRADE_DIM
                                      WHERE GRADE_NAME = 'AD'
                                        AND ROWNUM = 1),
                                    NULL,
                                    (SELECT GRADEID
                                       FROM GRADE_DIM
                                      WHERE GRADE_NAME = 'AD'
                                        AND ROWNUM = 1),
                                    i_GradeId)
             AND ASES.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
             AND SOM.FORMID = GSL.FORMID
             AND SOM.ASSESSMENTID = GSL.ASSESSMENTID
             AND SOM.LEVELID = GSL.LEVELID
             AND SOM.SUBTEST_CODE NOT IN (v_ELA,v_OverAllComp)

        UNION

        SELECT DISTINCT SOM.SUBTESTID, SOM.SUBTEST_NAME, SOM.SUBTEST_SEQ
          FROM MV_SUB_OBJ_FORM_MAP SOM,
               EDU_CENTER_DETAILS EDTLS
         WHERE  EDTLS.EDU_CENTERID = i_LoggedInUserJasperOrgId
           AND SOM.SUBTEST_CODE NOT IN (v_ELA,v_OverAllComp)
           AND EXISTS (SELECT 1
                  FROM SUBTEST_SCORE_FACT SCR, STUDENT_BIO_DIM STD , CUST_PRODUCT_LINK CUST
                 WHERE STD.ORG_NODEID = SCR.ORG_NODEID
                   AND STD.STUDENT_BIO_ID = SCR.STUDENT_BIO_ID
                   AND STD.EDU_CENTERID = EDTLS.EDU_CENTERID
                   AND STD.CUSTOMERID = EDTLS.CUSTOMERID
                   AND CUST.CUSTOMERID = EDTLS.CUSTOMERID
                   AND SCR.SUBTESTID = SOM.SUBTESTID
                   AND SCR.LEVELID = SOM.LEVELID
                   AND SCR.ASSESSMENTID = SOM.ASSESSMENTID
                   AND SCR.FORMID = SOM.FORMID
                   AND CUST.PRODUCTID = DECODE(i_ProductId,
                                       -99,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       NULL,
                                       (SELECT PRODUCTID
                                          FROM PRODUCT
                                         WHERE PRODUCT_SEQ = 1
                                           AND ROWNUM = 1),
                                       i_ProductId)
                    AND CUST.CUST_PROD_ID = SCR.CUST_PROD_ID
                    AND CUST.ADMINID = SCR.ADMINID
                   AND SCR.SS > 0)
         ORDER BY 3) A;


/*  CURSOR c_Get_Customer
  IS
  SELECT CUST.CUSTOMERID
   FROM CUSTOMER_INFO CUST,USERS USR
   WHERE UPPER(USR.USERNAME)=UPPER(i_LoggedInUserName)
     AND USR.CUSTOMERID = CUST.CUSTOMERID
     AND ROWNUM=1;*/


BEGIN
  IF i_AdminId = -99 THEN
    /*FOR r_Get_Customer IN c_Get_Customer
    LOOP*/
      v_Cust_Id := NULL ; --r_Get_Customer.CUSTOMERID;
      v_Date_End := to_char(trunc(SYSDATE),'MM/DD/RRRR');
      v_Date_Start := to_char(trunc(SYSDATE-30),'MM/DD/RRRR');
    /*END LOOP;*/
   ELSE
      v_Cust_Id := i_AdminId;
      v_Date_End := i_Start_Test_Date;
      v_Date_Start :=i_End_Test_Date;
  END IF;


  --------Single Select -------------------
  IF i_SubtestIdSingleSelect = -99 THEN
    FOR r_Get_Subtest_Obj_Login IN c_Get_Subtest_Obj_Login (v_Cust_Id,v_Date_Start,v_Date_End)
    LOOP
      v_Subtest_Id := r_Get_Subtest_Obj_Login.SUBTESTID;
         END LOOP;
   ELSE
      v_Subtest_Id := i_SubtestIdSingleSelect;
  END IF;


  -----Multiselect ----------------------
  IF i_SubtestIdMultiselect = '-99' AND i_type <> 3 THEN
    FOR r_Get_Subtest_Sub_Login IN c_Get_Subtest_Sub_Login (v_Cust_Id)
    LOOP
      v_SubtestMulti_Id := r_Get_Subtest_Sub_Login.SUBTEST_MULTI;
    END LOOP;
   ELSIF  i_SubtestIdMultiselect = '-99' AND i_type = 3 THEN
     FOR r_Get_Subtest_Hse_Login IN c_Get_Subtest_Hse_Login (v_Cust_Id)
      LOOP
        v_SubtestMulti_Id := r_Get_Subtest_Hse_Login.SUBTEST_MULTI;
      END LOOP;
   ELSE
      v_SubtestMulti_Id := i_SubtestIdMultiselect;
  END IF;



    --IF v_Cust_Id IS NOT NULL THEN
       IF i_type = 1  THEN

          FOR r_Get_Form_For_Obj IN c_Get_Form_For_Obj(v_Subtest_Id)
              LOOP
                   t_PRS_PGT_GLOBAL_TEMP_OBJ := PRS_PGT_GLOBAL_TEMP_OBJ();

                   t_PRS_PGT_GLOBAL_TEMP_OBJ.vc1 := r_Get_Form_For_Obj.FORMID;
                   t_PRS_PGT_GLOBAL_TEMP_OBJ.vc2 := r_Get_Form_For_Obj.FORM_NAME;

                   t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ.EXTEND(1);
                   t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ(t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ.COUNT):= t_PRS_PGT_GLOBAL_TEMP_OBJ;

              END LOOP;
        ELSE
            FOR r_Get_Form_For_Sub IN c_Get_Form_For_Sub(v_SubtestMulti_Id)
              LOOP
                   t_PRS_PGT_GLOBAL_TEMP_OBJ := PRS_PGT_GLOBAL_TEMP_OBJ();

                   t_PRS_PGT_GLOBAL_TEMP_OBJ.vc1 := r_Get_Form_For_Sub.FORMID;
                   t_PRS_PGT_GLOBAL_TEMP_OBJ.vc2 := r_Get_Form_For_Sub.FORM_NAME;

                   t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ.EXTEND(1);
                   t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ(t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ.COUNT):= t_PRS_PGT_GLOBAL_TEMP_OBJ;

              END LOOP;

       END IF;
   --END IF;

RETURN t_PRS_COLL_PGT_GLOBAL_TEMP_OBJ;
EXCEPTION
  WHEN OTHERS THEN
 -- RAISE;
  RETURN NULL;
END SF_GET_FORM;
/
