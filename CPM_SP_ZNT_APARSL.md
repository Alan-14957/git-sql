```sql
CREATE OR REPLACE PROCEDURE CPM_SP_ZNT_APARSL(IN P_SCE VARCHAR(7),
                                             IN P_PER VARCHAR(2),
                                             IN P_ENT VARCHAR(2000),
                                             IN P_USER VARCHAR(200))
LANGUAGE SQLSCRIPT AS
  /*=======================================================================
    文件名称 : CPM_SP_ZNT_APARSL
    描述     : 应收应付账龄附注表
    特别说明 :
    更改历史
    --------------------------
    版本         日期             创建人/修改人          描述
    ----- ---------  ------------- ------------------------------------------
    V1.0      2024-10-22            LYF                新建
 ========================================================================*/

BEGIN
DECLARE P_ENT_LIST VARCHAR(500); --公司参数list
   BEGIN
      /*获取公司参数清单*/
      IF :P_ENT = 'ALL' THEN
         SELECT STRING_AGG(COD_AZIENDA,',' ORDER BY COD_AZIENDA)
         INTO P_ENT_LIST
         FROM AZIENDA
         WHERE COD_AZIENDA <> '$'
         AND COD_AZIENDA <> 'ZZZE';
      ELSEIF P_ENT <> 'ALL' THEN
      P_ENT_LIST = :P_ENT;
      END IF;
   END;

---------------------------------------------------------------------------------------------------------------------
DELETE FROM AW_ZNT_ARAPSL_000001
WHERE SCE = P_SCE
AND PER = P_PER
AND INSTR(',' || P_ENT_LIST  || ',',',' || ENT_COD || ',') > 0
AND PROVENIENZA = 'CPM_SP_ZNT_APARSL';

/*=========================应收==============================*/
INSERT INTO AW_ZNT_ARAPSL_000001(OID,
      SCE,      --场景
      PER,      --期间
      ENT_COD,    --公司
      ACCOUNT,    --AW科目
      ACCT_CODE,    --科目号
      CTP_COD,    --客户编码
      CTP_DESC,   --客户名称
      ACCT_DESC,    --科目名称
      ACCT,     --报表科目
      PAY_NAT,    --款项性质
      RELAT_DEFIN,  --关系定义
      BAL_AMT,    --账面余额
      EB_3M_AMT,    --3个月以下（期末余额账龄）
      EB_6M_AMT,    --3-6个月（期末余额账龄）
      EB_12M_AMT,   --6-12个月（期末余额账龄）
      EB_2Y_AMT,    --1-2年（期末余额账龄）
      EB_3Y_AMT,    --2-3年（期末余额账龄）
      EB_4Y_AMT,    --3-4年（期末余额账龄）
      EB_5Y_AMT,    --4-5年（期末余额账龄）
      EB_O5Y_AMT,   --5年以上（期末余额账龄）
      BD_3M_AMT,    --3个月以下（坏账准备金额）
      BD_6M_AMT,    --3-6个月（坏账准备金额）
      BD_12M_AMT,   --6-12个月（坏账准备金额）
      BD_2Y_AMT,    --1-2年（坏账准备金额）
      BD_3Y_AMT,    --2-3年（坏账准备金额）
      BD_4Y_AMT,    --3-4年（坏账准备金额）
      BD_5Y_AMT,    --4-5年（坏账准备金额）
      BD_O5Y_AMT,   --5年以上（坏账准备金额）
      BD_SUM_AMT,   --小计（坏账准备金额）
      EB_NET_AMT,   --期末净值
      PROVENIENZA,
      DATEUPD,
      USERUPD)

/* 新增=====【附注】应收账龄 ========*/
/*=====================应收附注（科目余额重分类数据）=================*/
SELECT NEWID() OID,
 P_SCE                                               AS SCE                  -- 场景
,P_PER                                             AS PER                  -- 期间
,ENTITY                                                  AS ENT_COD              -- 公司
,'NT_ACC_AR'                                             AS ACCOUNT              -- AW科目
,EPM_ACC_COD                                             AS ACCT_CODE            -- 科目号
,NULL                                                    AS CTP_COD              -- 客户编码
,NULL                                                    AS CTP_DESC             -- 客户名称
,EPM_ACC_DES                                             AS ACCT_DESC            -- 科目名称
, SUBSTRING(EPM_ACC_DES, 1, INSTR(EPM_ACC_DES, '-') - 1) AS ACCT                 -- 报表科目
,c.ATTRIBUTO1                                            AS PAY_NAT              -- 款项性质
,NULL                                                    AS RELAT_DEFIN          -- 关系定义
,ENDBL                                                   AS BAL_AMT              -- 账面余额
,ENDBL                                                   AS EB_3M_AMT            -- 3个月以下（期末余额账龄）
,0                                                       AS EB_6M_AMT            -- 3-6个月（期末余额账龄）
,0                                                       AS EB_12M_AMT           -- 6-12个月（期末余额账龄）
,0                                                       AS EB_2Y_AMT            -- 1-2年（期末余额账龄）
,0                                                       AS EB_3Y_AMT            -- 2-3年（期末余额账龄）
,0                                                       AS EB_4Y_AMT            -- 3-4年（期末余额账龄）
,0                                                       AS EB_5Y_AMT            -- 4-5年（期末余额账龄）
,0                                                       AS EB_O5Y_AMT           -- 5年以上（期末余额账龄）
,0                                                       AS BD_3M_AMT            -- 3个月以下（坏账准备金额）
,0                                                       AS BD_6M_AMT            -- 3-6个月（坏账准备金额）
,0                                                       AS BD_12M_AMT           -- 6-12个月（坏账准备金额）
,0                                                       AS BD_2Y_AMT            -- 1-2年（坏账准备金额）
,0                                                       AS BD_3Y_AMT            -- 2-3年（坏账准备金额）
,0                                                       AS BD_4Y_AMT            -- 3-4年（坏账准备金额）
,0                                                       AS BD_5Y_AMT            -- 4-5年（坏账准备金额）
,0                                                       AS BD_O5Y_AMT           -- 5年以上（坏账准备金额）
,0                                                       AS BD_SUM_AMT           -- 小计（坏账准备金额）
,ENDBL                                                       AS EB_NET_AMT           -- 期末净值
,'CPM_SP_ZNT_APARSL'                                     AS PROVENIENZA
,NOW()                                                   AS DATEUPD
,:P_USER                                                 AS USERUPD
FROM AW_ODS_GLBALC_000001 glbalc
LEFT JOIN(SELECT COD_CONTO,F_TGK_UNICODE('decode',ATTRIBUTO1) AS ATTRIBUTO1 FROM CONTO)C
ON glbalc.EPM_ACC_COD = C.COD_CONTO
WHERE LEFT(EPM_ACC_COD,4) IN ('1122','1123','1221','1531')
  AND LEFT(CATEGORY,4) = '1REC'
  AND SCE = LEFT(P_SCE,4)
  AND PER = P_PER
  AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0

UNION ALL

/* ==================应收附注（账龄表取数）=================  */
SELECT NEWID() OID,
       SCE,      --场景
       PER,      --期间
       ENT_COD,    --公司
       ACCOUNT,    --AW科目
       ACCT_CODE,    --科目号
       CTP_COD,    --客户编码
       CTP_DESC,   --客户名称
       ACCT_DESC,    --科目名称
       ACCT,     --报表科目
       PAY_NAT,    --款项性质
       RELAT_DEFIN,  --关系定义
       BAL_AMT,    --账面余额
       EB_3M_AMT,    --3个月以下（期末余额账龄）
       EB_6M_AMT,    --3-6个月（期末余额账龄）
       EB_12M_AMT,   --6-12个月（期末余额账龄）
       EB_2Y_AMT,    --1-2年（期末余额账龄）
       EB_3Y_AMT,    --2-3年（期末余额账龄）
       EB_4Y_AMT,    --3-4年（期末余额账龄）
       EB_5Y_AMT,    --4-5年（期末余额账龄）
       EB_O5Y_AMT,   --5年以上（期末余额账龄）
       BD_3M_AMT,    --3个月以下（坏账准备金额）
       BD_6M_AMT,    --3-6个月（坏账准备金额）
       BD_12M_AMT,   --6-12个月（坏账准备金额）
       BD_2Y_AMT,    --1-2年（坏账准备金额）
       BD_3Y_AMT,    --2-3年（坏账准备金额）
       BD_4Y_AMT,    --3-4年（坏账准备金额）
       BD_5Y_AMT,    --4-5年（坏账准备金额）
       BD_O5Y_AMT,   --5年以上（坏账准备金额）
       (BD_3M_AMT + BD_6M_AMT + BD_12M_AMT + BD_2Y_AMT + BD_3Y_AMT + BD_4Y_AMT + BD_5Y_AMT + BD_O5Y_AMT) BD_SUM_AMT,   --小计（坏账准备金额）
      BAL_AMT - (IFNULL(BD_3M_AMT,0) + IFNULL(BD_6M_AMT,0) + IFNULL(BD_12M_AMT,0) + IFNULL(BD_2Y_AMT,0) + IFNULL(BD_3Y_AMT,0) + IFNULL(BD_4Y_AMT,0) + IFNULL(BD_5Y_AMT,0) + IFNULL(BD_O5Y_AMT,0)) EB_NET_AMT,   --期末净值
       'CPM_SP_ZNT_APARSL' PROVENIENZA,
       NOW() DATEUPD,
       :P_USER USERUPD FROM
    (SELECT P_SCE  SCE,
           P_PER PER,
           SRC.ENTITY ENT_COD,
           'NT_ACC_AR' ACCOUNT,
           SRC.EPM_ACC_COD ACCT_CODE,
           SRC.CS_COD CTP_COD,
           SRC.CS_DESC CTP_DESC,
           SRC.EPM_ACC_DESC ACCT_DESC,
           SUBSTRING(SRC.EPM_ACC_DESC, 1, LOCATE(SRC.EPM_ACC_DESC,'-') - 1) ACCT,
           F_TGK_UNICODE('decode',CONTO.ATTRIBUTO1) PAY_NAT,
           CS.GLXZ RELAT_DEFIN,
           SRC.ENDBL BAL_AMT,
           SRC.AG01 EB_3M_AMT,
           SRC.AG02 EB_6M_AMT,
           SRC.AG03 EB_12M_AMT,
           SRC.AG04 EB_2Y_AMT,
           SRC.AG05 EB_3Y_AMT,
           SRC.AG06 EB_4Y_AMT,
           SRC.AG07 EB_5Y_AMT,
           SRC.AG08 EB_O5Y_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG01 * MAP.IMPORTO END BD_3M_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG02 * MAP.IMPORTO_1 END BD_6M_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG03 * MAP.IMPORTO_2 END BD_12M_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG04 * MAP.IMPORTO_3 END BD_2Y_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG05 * MAP.IMPORTO_4 END BD_3Y_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG06 * MAP.IMPORTO_5 END BD_4Y_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG07 * MAP.IMPORTO_6 END BD_5Y_AMT,
           CASE WHEN LEFT(EPM_ACC_COD,4) = '1123' THEN 0 ELSE SRC.AG08 * MAP.IMPORTO_7 END BD_O5Y_AMT
           FROM AW_ODS_APARAG_000001 SRC
           LEFT JOIN CONTO
           ON SRC.EPM_ACC_COD = COD_CONTO
           LEFT JOIN ( SELECT KSCODE,GLXZ FROM
               (SELECT DISTINCT KSCODE,GLXZ,ROW_NUMBER() OVER(PARTITION BY KSCODE) AS RN FROM AW_ODS_CSRPDM_000001)
               WHERE RN = 1) CS
           ON SRC.CS_COD = CS.KSCODE
           LEFT JOIN FORM_DATI MAP
           ON SRC.EPM_ACC_COD = LTRIM(MAP.TESTO_1,'0') AND MAP.COD_CONTO = 'ZTF_RULE_BAD' AND MAP.COD_SCENARIO = P_SCE AND MAP.COD_PERIODO = P_PER
           WHERE SCE = LEFT(P_SCE,4)
           AND PER = P_PER
           AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0
           AND LEFT(EPM_ACC_COD,4) IN ('1122','1123','1221','1531')
       ) SRC
;

/*=========================应付==============================*/
INSERT INTO AW_ZNT_ARAPSL_000001(OID,
       SCE,      --场景
       PER,      --期间
       ENT_COD,    --公司
       ACCOUNT,    --AW科目
       ACCT_CODE,    --科目号
       CTP_COD,    --客户编码
       CTP_DESC,   --客户名称
       ACCT_DESC,    --科目名称
       ACCT,     --报表科目
       PAY_NAT,    --款项性质
       RELAT_DEFIN,  --关系定义
       AP_AMT,     --应付账款余额
       EP_ACCR_AMT,  --应付暂估-预提
       EP_TRANS_AMT, --应付暂估-关联方资产转移
       EP_OT_AMT,    --应付暂估-其他
       EP_AMT,       --应付暂估小计
       BAL_AMT,      --账面余额
       EB_3M_AMT,    --3个月以下（期末余额账龄）
       EB_6M_AMT,    --3-6个月（期末余额账龄）
       EB_12M_AMT,   --6-12个月（期末余额账龄）
       EB_2Y_AMT,    --1-2年（期末余额账龄）
       EB_3Y_AMT,    --2-3年（期末余额账龄）
       EB_4Y_AMT,    --3-4年（期末余额账龄）
       EB_5Y_AMT,    --4-5年（期末余额账龄）
       EB_O5Y_AMT,   --5年以上（期末余额账龄）
       PROVENIENZA,
       DATEUPD,
       USERUPD)
/* 新增=============【附注】应付账龄 ============== */
/*=====================应付附注（科目余额重分类数据）=================*/
SELECT NEWID() OID,
 P_SCE                               AS SCE                  -- 场景
,P_PER                                             AS PER                  -- 期间
,ENTITY                                                  AS ENT_COD              -- 公司
,'NT_ACC_AP'                                             AS ACCOUNT              -- AW科目
,EPM_ACC_COD                                             AS ACCT_CODE            -- 科目号
,NULL                                                    AS CTP_COD              -- 客户编码
,NULL                                                    AS CTP_DESC             -- 客户名称
,EPM_ACC_DES                                             AS ACCT_DESC            -- 科目名称
, SUBSTRING(EPM_ACC_DES, 1, INSTR(EPM_ACC_DES, '-') - 1) AS ACCT                 -- 报表科目
,c.ATTRIBUTO1                                            AS PAY_NAT              -- 款项性质
,NULL                                                    AS RELAT_DEFIN              -- 关系定义
,CASE
      WHEN LEFT(EPM_ACC_COD,4) IN ('2202')  THEN ENDBL
      ELSE 0
 END                                                     AS AP_AMT          -- 应付账款余额
,0                                                       AS EP_ACCR_AMT     -- 应付-预提
,0                                                       AS EP_TRANS_AMT    -- 应付暂估-设备
,0                                                       AS EP_OT_AMT       -- 应付暂估-其他
,0                                                       AS EP_AMT          -- 应付暂估小计
,ENDBL                                                   AS BAL_AMT         -- 账面余额
,ENDBL                                                   AS EB_3M_AMT       -- 3个月以下（期末余额账龄）
,0                                                       AS EB_6M_AMT       -- 3-6个月（期末余额账龄）
,0                                                       AS EB_12M_AMT      -- 6-12个月（期末余额账龄）
,0                                                       AS EB_2Y_AMT       -- 1-2年（期末余额账龄）
,0                                                       AS EB_3Y_AMT       -- 2-3年（期末余额账龄）
,0                                                       AS EB_4Y_AMT       -- 3-4年（期末余额账龄）
,0                                                       AS EB_5Y_AMT       -- 4-5年（期末余额账龄）
,0                                                       AS EB_O5Y_AMT      -- 5年以上（期末余额账龄）
,'CPM_SP_ZNT_APARSL'                                     AS PROVENIENZA
,NOW()                                                   AS DATEUPD
,:P_USER                                                 AS USERUPD
FROM AW_ODS_GLBALC_000001 glbalc
LEFT JOIN(SELECT COD_CONTO,F_TGK_UNICODE('decode',ATTRIBUTO1) AS ATTRIBUTO1 FROM CONTO)C
ON glbalc.EPM_ACC_COD = C.COD_CONTO
WHERE LEFT(EPM_ACC_COD,4) IN ('2202','2203','2241','2701')
  AND LEFT(CATEGORY,4) = '1REC'
  AND SCE = LEFT(P_SCE,4)
  AND PER = P_PER
  AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0

UNION ALL



/*===========应付(【标准层】账龄表取数)取暂估到列上数据处理=============*/
SELECT NEWID() OID,
       SCE,
       PER,
       ENT_COD,
       ACCOUNT,
       ACCT_CODE,
       CTP_COD,
       CTP_DESC,
       ACCT_DESC,
       ACCT,
       PAY_NAT,
       RELAT_DEFIN,
       AP_AMT,
       EP_ACCR_AMT,
       EP_TRANS_AMT,
       EP_OT_AMT,
       IFNULL(EP_ACCR_AMT,0) + IFNULL(EP_TRANS_AMT,0)  + IFNULL(EP_OT_AMT,0)  EP_AMT,           --暂估小计
       IFNULL(AP_AMT,0)  + IFNULL(EP_ACCR_AMT,0)  + IFNULL(EP_TRANS_AMT,0)  + IFNULL(EP_OT_AMT,0)  AS  BAL_AMT,     --账面余额合计
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG01 ELSE AG01 END  EB_3M_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG02 ELSE AG02 END  EB_6M_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG03 ELSE AG03 END  EB_12M_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG04 ELSE AG04 END  EB_2Y_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG05 ELSE AG05 END  EB_3Y_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG06 ELSE AG06 END  EB_4Y_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG07 ELSE AG07 END  EB_5Y_AMT,
       CASE WHEN ACCT_CODE IN ('2202010201','2202010203','2202010200') THEN AG08 ELSE AG08 END  EB_O5Y_AMT,
       'CPM_SP_ZNT_APARSL' PROVENIENZA,
       NOW() DATEUPD,
       :P_USER USERUPD
       FROM
     (SELECT       P_SCE   SCE,
             P_PER PER,
             coalesce(SRC1.ENTITY,SRC2.ENTITY,SRC3.ENTITY) ENT_COD,
             'NT_ACC_AP' ACCOUNT,
             coalesce(SRC1.EPM_ACC_COD,SRC2.EPM_ACC_COD,SRC3.EPM_ACC_COD) ACCT_CODE,
             coalesce(SRC1.CS_COD,SRC2.CS_COD,SRC3.CS_COD) CTP_COD,
             coalesce(SRC1.CS_DESC,SRC2.CS_DESC,SRC3.CS_DESC) CTP_DESC,
             coalesce(SRC1.EPM_ACC_DESC,SRC2.EPM_ACC_DESC,SRC3.EPM_ACC_DESC) ACCT_DESC,
            CASE WHEN coalesce(SRC1.EPM_ACC_DESC,SRC2.EPM_ACC_DESC,SRC3.EPM_ACC_DESC) = '应付账款-暂估' THEN '应付暂估' ELSE  coalesce(SUBSTRING(SRC1.EPM_ACC_DESC, 1, LOCATE(SRC1.EPM_ACC_DESC, '-') - 1),SUBSTRING(SRC2.EPM_ACC_DESC, 1, LOCATE(SRC2.EPM_ACC_DESC, '-') - 1),SUBSTRING(SRC3.EPM_ACC_DESC, 1, LOCATE(SRC3.EPM_ACC_DESC, '-') - 1)) END AS ACCT,/*20241207修改'应付账款-暂估' THEN '应付暂估' */
             coalesce(SRC1.PAY_NAT,SRC2.KXXZ,SRC3.KXXZ)   PAY_NAT,
             CS.GLXZ RELAT_DEFIN,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-SRC1.ENDBL ELSE 0-SRC1.ENDBL END   AS AP_AMT,
             0-IFNULL(SRC2.DMBTR1,0)        AS EP_ACCR_AMT,   --应付-预提
             0-IFNULL(SRC2.DMBTR2,0)        AS EP_TRANS_AMT,  --应付-设备
             0-IFNULL(SRC3.DMBTR3,0)            AS EP_OT_AMT,     --应付账款-暂估
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG01,0) + IFNULL(SRC2.ZG_AG01,0) + IFNULL(SRC3.ZG_AG01,0)) ELSE 0-(IFNULL(SRC1.AG01,0) + IFNULL(SRC2.ZG_AG01,0) + IFNULL(SRC3.ZG_AG01,0))END AG01,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG02,0) + IFNULL(SRC2.ZG_AG02,0) + IFNULL(SRC3.ZG_AG02,0)) ELSE 0-(IFNULL(SRC1.AG02,0) + IFNULL(SRC2.ZG_AG02,0) + IFNULL(SRC3.ZG_AG02,0))END AG02,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG03,0) + IFNULL(SRC2.ZG_AG03,0) + IFNULL(SRC3.ZG_AG03,0)) ELSE 0-(IFNULL(SRC1.AG03,0) + IFNULL(SRC2.ZG_AG03,0) + IFNULL(SRC3.ZG_AG03,0))END AG03,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG04,0) + IFNULL(SRC2.ZG_AG04,0) + IFNULL(SRC3.ZG_AG04,0)) ELSE 0-(IFNULL(SRC1.AG04,0) + IFNULL(SRC2.ZG_AG04,0) + IFNULL(SRC3.ZG_AG04,0))END AG04,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG05,0) + IFNULL(SRC2.ZG_AG05,0) + IFNULL(SRC3.ZG_AG05,0)) ELSE 0-(IFNULL(SRC1.AG05,0) + IFNULL(SRC2.ZG_AG05,0) + IFNULL(SRC3.ZG_AG05,0))END AG05,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG06,0) + IFNULL(SRC2.ZG_AG06,0) + IFNULL(SRC3.ZG_AG06,0)) ELSE 0-(IFNULL(SRC1.AG06,0) + IFNULL(SRC2.ZG_AG06,0) + IFNULL(SRC3.ZG_AG06,0))END AG06,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG07,0) + IFNULL(SRC2.ZG_AG07,0) + IFNULL(SRC3.ZG_AG07,0)) ELSE 0-(IFNULL(SRC1.AG07,0) + IFNULL(SRC2.ZG_AG07,0) + IFNULL(SRC3.ZG_AG07,0))END AG07,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-(IFNULL(SRC1.AG08,0) + IFNULL(SRC2.ZG_AG08,0) + IFNULL(SRC3.ZG_AG08,0)) ELSE 0-(IFNULL(SRC1.AG08,0) + IFNULL(SRC2.ZG_AG08,0) + IFNULL(SRC3.ZG_AG08,0))END AG08
FROM (SELECT APARAG.*,F_TGK_UNICODE('decode',CONTO.ATTRIBUTO1) PAY_NAT FROM AW_ODS_APARAG_000001 APARAG
       LEFT JOIN CONTO
       ON EPM_ACC_COD = COD_CONTO
        WHERE SCE = LEFT(P_SCE,4)
      AND PER = P_PER
      AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0
          AND LEFT(EPM_ACC_COD,4) IN ('2202','2203','2241','2701')
      AND EPM_ACC_COD NOT IN ('2202010201','2202010203','2202010200')
          AND CATEGORY = '0ETL'  ) SRC1

             LEFT JOIN ( SELECT KSCODE,GLXZ FROM
               (SELECT DISTINCT KSCODE,GLXZ,ROW_NUMBER() OVER(PARTITION BY KSCODE) AS RN FROM AW_ODS_CSRPDM_000001)
               WHERE RN = 1) CS
           ON SRC1.CS_COD = CS.KSCODE
             FULL JOIN
             (SELECT   EPM_ACC_COD,EPM_ACC_DESC,
            ENTITY
            ,CS_COD
                    ,CS_DESC
          , KXXZ
                    ,SUM(AG01) AS ZG_AG01
                    ,SUM(AG02) AS ZG_AG02
                    ,SUM(AG03) AS ZG_AG03
                    ,SUM(AG04) AS ZG_AG04
                    ,SUM(AG05) AS ZG_AG05
                    ,SUM(AG06) AS ZG_AG06
                    ,SUM(AG07) AS ZG_AG07
                    ,SUM(AG08) AS ZG_AG08
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010201' AND KXXZ = '费用' THEN ENDBL ELSE 0 END ) DMBTR1
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010203' AND KXXZ  IN ('设备','其他长期资产','资产') THEN ENDBL ELSE 0 END ) DMBTR2
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010200' THEN ENDBL ELSE 0 END ) DMBTR3

          FROM AW_ODS_APARAG_000001
               WHERE SCE = LEFT(P_SCE,4)
           AND PER = P_PER
         AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0
         AND EPM_ACC_COD IN ('2202010201','2202010203')
         AND LEFT(CATEGORY,1) <> '3' --排除3开头的类别
               GROUP BY ENTITY, CS_COD, KXXZ,CS_DESC,EPM_ACC_COD,EPM_ACC_DESC  ) SRC2
              ON IFNULL(SRC1.ENTITY,'1') = IFNULL(SRC2.ENTITY,'1')
           AND IFNULL(SRC1.CS_COD ,'1')= IFNULL(SRC2.CS_COD,'1')
             AND IFNULL(SRC1.PAY_NAT,'1') =IFNULL( SRC2.KXXZ,'1')

  FULL JOIN
             (SELECT   EPM_ACC_COD,EPM_ACC_DESC,
            ENTITY
            ,CS_COD
                    ,CS_DESC
          , KXXZ
                    ,SUM(AG01) AS ZG_AG01
                    ,SUM(AG02) AS ZG_AG02
                    ,SUM(AG03) AS ZG_AG03
                    ,SUM(AG04) AS ZG_AG04
                    ,SUM(AG05) AS ZG_AG05
                    ,SUM(AG06) AS ZG_AG06
                    ,SUM(AG07) AS ZG_AG07
                    ,SUM(AG08) AS ZG_AG08
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010201' AND KXXZ = '费用' THEN ENDBL ELSE 0 END ) DMBTR1
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010203' AND KXXZ IN ('设备','其他长期资产','资产') THEN ENDBL ELSE 0 END ) DMBTR2
          ,SUM(CASE WHEN EPM_ACC_COD = '2202010200' THEN ENDBL ELSE 0 END ) DMBTR3

          FROM AW_ODS_APARAG_000001
               WHERE SCE = LEFT(P_SCE,4)
                 AND PER = P_PER
                 AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0
                 AND EPM_ACC_COD IN ('2202010200')
                 AND LEFT(CATEGORY,1) <> '3' --排除3开头的类别
               GROUP BY ENTITY, CS_COD, KXXZ,CS_DESC,EPM_ACC_COD,EPM_ACC_DESC  ) SRC3
                ON IFNULL(SRC1.ENTITY,'1') = IFNULL(SRC3.ENTITY,'1')
           AND IFNULL(SRC1.CS_COD ,'1')= IFNULL(SRC3.CS_COD,'1')
             AND IFNULL(SRC1.PAY_NAT,'1') =IFNULL( SRC3.KXXZ,'1')

UNION ALL
/*==========重分类数据不做额外处理=============*/
SELECT       P_SCE   SCE,
             P_PER PER,
             SRC1.ENTITY ENT_COD,
             'NT_ACC_AP' ACCOUNT,
             SRC1.EPM_ACC_COD ACCT_CODE,
             SRC1.CS_COD CTP_COD,
             SRC1.CS_DESC CTP_DESC,
             SRC1.EPM_ACC_DESC ACCT_DESC,
             SUBSTRING(SRC1.EPM_ACC_DESC, 1, LOCATE(SRC1.EPM_ACC_DESC, '-') - 1) ACCT,
             F_TGK_UNICODE('decode',CONTO.ATTRIBUTO1) PAY_NAT,
             CS.GLXZ RELAT_DEFIN,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-SRC1.ENDBL ELSE 0-SRC1.ENDBL END   AS AP_AMT,
             0          AS EP_ACCR_AMT,   --应付-预提
             0          AS EP_TRANS_AMT,  --应付-设备
             0            AS EP_OT_AMT,     --应付账款-暂估
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG01,0)  ELSE 0-IFNULL(SRC1.AG01,0) END AG01,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG02,0)  ELSE 0-IFNULL(SRC1.AG02,0) END AG02,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG03,0)  ELSE 0-IFNULL(SRC1.AG03,0) END AG03,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG04,0)  ELSE 0-IFNULL(SRC1.AG04,0) END AG04,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG05,0)  ELSE 0-IFNULL(SRC1.AG05,0) END AG05,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG06,0)  ELSE 0-IFNULL(SRC1.AG06,0) END AG06,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG07,0)  ELSE 0-IFNULL(SRC1.AG07,0) END AG07,
             CASE WHEN SRC1.SYS_ID = 'SAP' THEN 0-IFNULL(SRC1.AG08,0)  ELSE 0-IFNULL(SRC1.AG08,0) END AG08
FROM
(
SELECT APARAG.*,F_TGK_UNICODE('decode',CONTO.ATTRIBUTO1) PAY_NAT FROM AW_ODS_APARAG_000001 APARAG
        LEFT JOIN CONTO
        ON EPM_ACC_COD = COD_CONTO
        WHERE SCE = LEFT(P_SCE,4)
      AND PER = P_PER
      AND INSTR(',' || P_ENT_LIST  || ',',',' || ENTITY || ',') > 0
          AND LEFT(EPM_ACC_COD,4) IN ('2202','2203','2241','2701')
      AND EPM_ACC_COD NOT IN ('2202010201','2202010203','2202010200')
          AND LEFT(CATEGORY,4) = '1REC'
)SRC1 -- 重分类数据
LEFT JOIN CONTO
             ON SRC1.EPM_ACC_COD = COD_CONTO
LEFT JOIN ( SELECT KSCODE,GLXZ FROM
               (SELECT DISTINCT KSCODE,GLXZ,ROW_NUMBER() OVER(PARTITION BY KSCODE) AS RN FROM AW_ODS_CSRPDM_000001)
               WHERE RN = 1) CS
           ON SRC1.CS_COD = CS.KSCODE
             ) SRC
WHERE (
    AP_AMT <> 0 AND AP_AMT IS NOT NULL ) OR
    ( EP_ACCR_AMT <> 0 AND EP_ACCR_AMT IS NOT NULL ) OR
    (EP_TRANS_AMT <> 0 AND EP_TRANS_AMT IS NOT NULL )  OR
    (EP_OT_AMT <> 0 AND EP_OT_AMT IS NOT NULL )  OR
    (AG01 <> 0 AND AG01 IS NOT NULL )  OR
    (AG02 <> 0 AND AG02 IS NOT NULL )  OR
    (AG03 <> 0 AND AG03 IS NOT NULL )  OR
    (AG04 <> 0 AND AG04 IS NOT NULL )  OR
    (AG05 <> 0 AND AG05 IS NOT NULL )  OR
    (AG06 <> 0 AND AG06 IS NOT NULL )  OR
    (AG07 <> 0 AND AG07 IS NOT NULL )  OR
    (AG08 <> 0 AND AG08 IS NOT NULL )
 ;




/*新增数据源为往来分录 2ADJ的数据*/
INSERT INTO AW_ZNT_ARAPSL_000001(OID,
       SCE,      --场景
       PER,      --期间
       ENT_COD,    --公司
       ACCOUNT,    --AW科目
       ACCT_CODE,    --科目号
       CTP_COD,    --客户编码
       CTP_DESC,   --客户名称
       ACCT_DESC,    --科目名称
       ACCT,     --报表科目
       PAY_NAT,    --款项性质
       RELAT_DEFIN,  --关系定义
       BAL_AMT,      --账面余额
       EB_3M_AMT,    --3个月以下（期末余额账龄）
       PROVENIENZA,
       DATEUPD,
       USERUPD)
SELECT NEWID(),
       D.COD_SCENARIO SCE,      --场景
       D.COD_PERIODO PER,      --期间
       D.COD_AZIENDA ENT_COD,    --公司
       CASE WHEN LEFT(D.COD_CONTO,1) = '1' THEN 'NT_ACC_AR' ELSE 'NT_ACC_AP' END ACCOUNT,    --AW科目
       D.COD_CONTO ACCT_CODE,    --科目号
       D.COD_AZI_CTP CTP_COD,    --客户编码
       A.DESC_AZIENDA0 CTP_DESC,   --客户名称
       C.DESC_CONTO0 ACCT_DESC,    --科目名称
       NULL ACCT,     --报表科目
       NULL PAY_NAT,    --款项性质
       NULL RELAT_DEFIN,  --关系定义
       D.IMPORTO BAL_AMT,      --账面余额
       D.IMPORTO EB_3M_AMT,    --3个月以下（期末余额账龄）
       'CPM_SP_ZNT_APARSL' PROVENIENZA,
       NOW() DATEUPD,
       :P_USER USERUPD FROM
       DATI_RETT_RIGA D
       LEFT JOIN CONTO C
       ON D.COD_CONTO = C.COD_CONTO
       LEFT JOIN AZIENDA A
       ON D.COD_AZI_CTP = A.COD_AZIENDA
       WHERE D.COD_SCENARIO = P_SCE
       AND D.COD_PERIODO = P_PER
       AND INSTR(',' || P_ENT_LIST  || ',',',' || D.COD_AZIENDA || ',') > 0
       AND LEFT(D.COD_CONTO,4) IN ('1122','1123','1221','1531','2202','2203','2241','2701')
       AND LEFT(D.COD_CATEGORIA,1) = '2';


END;
```

