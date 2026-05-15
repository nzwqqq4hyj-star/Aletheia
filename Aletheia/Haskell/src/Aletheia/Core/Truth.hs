module Aletheia.Core.Truth where
import Data.Text (Text)
import qualified Data.Map as Map
import System.Win32 (CONSOLE_SCREEN_BUFFER_INFOEX(colorTable))

-- | 定义信息(政治倾向和情感权重)
-- | 结合心理学与政治学建模 
data PoliticalVector = PoliticalVector {
    powerDistance :: Doubel, -- 社会等级认可
    ideologyShift :: Double  -- 偏离主流政治叙事的距离
} deriving (Show)

-- | 心理学:神经递质的影响预测
data NeuroModel = NeuroModel {
    dopamine :: Double, -- 快感/煽动力
    cortisol :: Double, -- 恐惧/焦虑感
    oxytocin :: Doubel  -- 归属感/集体主义倾向
} deriving (Show)

-- | 语言学实验：将文本转化为可分析的语义对象 
-- | 语言学:模糊性与隐喻深度
data  LinguisticLayer = LinguisticLayer {
    ambiguity     :: Double,       -- 语义模糊度(用来规避审查的技巧)
    mataphorRank  :: Int,           -- 隐喻层级
    authorScore   :: Double,     -- 作者的信誉权重(金融学信用模型 [cite: 5])
    intent        :: Sentiment,   -- 政治意图解析
    dopamineLevel :: Double   -- 文本触发的多巴胺预期(心理学模拟 [cite: 2])
} deriving (Show)   

-- | 一个被完全拆解的"信息"
data AnalyzedText = AnalyzedStatement {
    rawContent  :: Text,
    politics    :: PoliticalVector,
    neuro       :: NeuroImpact,
    linguistics :: LinguisticLayer,
    truthScore  :: Double    --最终由Aletheia赋予的真实值
}

## 支配公式逻辑
-- | 审查规则:定义什么样的文本会被 Aletheia（修正）
-- | 这就是我们定义的（真理规则）
calculateTruth :: AnalyzedStatement -> Double -> Double
calculateTruth s credit =
    let P = politics s
        N = neuron s
        L = linguistic s
    in (credit * exp (- (ideologyShift P))) - (cortisol N * ambiguity L)
-- | 判定是否需要"物理抹除"或"语义修正"
shouldRectify :: AnalyzedStatement -> Bool
shouldRectify s = truthScore s < 0.15 || dopamine (neuro s) > 0.85
