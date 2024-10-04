import Text.Read (readMaybe, Lexeme (String))
import System.IO (hClose, hGetContents, openFile, hPrint, IOMode(ReadMode, AppendMode))
import Data.List (sort)
import Data.Data (Data)
import Data.Int ()

data Date = Date { year :: Int, month :: Int, day :: Int} deriving (Show, Eq, Read)
data Insurance = Insurance { stateNumberVehicle :: String, typeInsurance :: String, timeInsurance :: Int, priceInsurance :: Double, startDate :: Date } deriving (Show, Eq, Read)

validateDate :: Int -> Int -> Int -> Bool
validateDate day month year
  | (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && day >= 1 && day <= 31 = True
  | (month == 4 || month == 6 || month == 9 || month == 11) && day >= 1 && day <= 30 = True
  | year `mod` 4 == 0 && day >= 1 && day <= 29 = True
  | month == 2 && day >= 1 && day <= 28 = True
  | otherwise = False

inputDay :: IO Int
inputDay = do
  putStrLn "\nВведите день"
  input <- getLine
  case reads input of
    [(number, "")] -> if number <=0
            then do 
            putStrLn "Число не может быть отрицательным!"
            inputDay
            else if number > 31
                 then do 
                 putStrLn "Ни в одном месяце нет больше 31 дней!"
                 inputDay
                      else return number
    _ -> do
      putStrLn "Введите только число"
      inputDay

inputMonth :: IO Int
inputMonth = do
  putStrLn "\nВведите месяц"
  input <- getLine
  case reads input of
    [(number, "")] -> if number <=0
            then do 
            putStrLn "Месяц не может быть отрицательным!"
            inputMonth
            else if number > 12
                 then do 
                 putStrLn "В году только 12 месяцев!"
                 inputMonth
                      else return number
    _ -> do
      putStrLn "Введите только число"
      inputMonth

inputYear :: IO Int
inputYear = do
  putStrLn "\nВведите год"
  input <- getLine
  case reads input of
    [(number, "")] -> if number <=0
            then do 
            putStrLn "Год не может быть отрицательным!"
            inputYear
            else if number > 2030
                 then do 
                 putStrLn "Это слишком далёкое будущее, повторите попытку"
                 inputYear
            else if number < 2000
                 then do 
                 putStrLn "Наша компания была основана лишь в 2000 году, повторите ввод"
                 inputYear
                      else return number
    _ -> do
      putStrLn "Введите только число"
      inputYear

createNewDate :: IO Date
createNewDate = do
  d <- inputDay
  m <- inputMonth
  y <- inputYear
  if validateDate d m y
    then return Date {day = d, month = m, year = y}
    else do
      putStrLn "Количество дней не соотвествует месяцу, повторите попытку"
      createNewDate

service :: IO ()
service = do
  putStrLn "\nВыберите услугу:\n1 - информирование\n2 - рассчитать стоимость страховки\n3 - рассчитать возвратную сумму при досрочном расторжении договора"
  serviceInput <- getLine
  case readMaybe serviceInput :: Maybe Int of
    Just 1 -> do
      information
    Just 2 -> do
      calculatorTheCostOfInsurance
    Just 3 -> do
      recalculationService
    _ -> do 
      putStrLn "Введите только число 1, 2 или 3" 
      service

calculatorTheCostOfInsurance :: IO ()
calculatorTheCostOfInsurance = do
    putStrLn "\nКакое страхование необходимо рассчитать:\n1 - ОСАГО\n2 - КАСКО"
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just 1 -> printForOSAGO
        Just 2 -> printForKASKO
        _ -> do
           putStrLn "Некорректный ввод. Пожалуйста, выберите 1 или 2"
           calculatorTheCostOfInsurance

priceOSAGO :: IO (Double, Int)
priceOSAGO = do
    userExperience <- experience
    userAge <- age
    if validAgeAndExperience (snd userAge) (snd userExperience) then do
      userPrice <- price
      userTypeAuto <- typeAuto
      userPower <- power
      userTimeIns <- timeOSAGO
      userBenifit <- benifit
      let a = userPrice * (fst userAge) * (fst userExperience) * userTypeAuto * userPower * fst userTimeIns * userBenifit
      putStrLn "\nСтоимость ОСАГО равно: "
      print a
      return (a, snd userTimeIns) 
    else do
      putStrLn "Вы не могли получить водительское удостоверение раньше 18 лет, повторите попытку"
      priceKASKO

validAgeAndExperience :: Int -> Int -> Bool
validAgeAndExperience years driveExperience = 
  if (years - driveExperience) < 17 then False
  else True

priceKASKO :: IO (Double, Int)
priceKASKO = do
    userExperience <- experience
    userAge <- age
    if validAgeAndExperience (snd userAge) (snd userExperience) then do 
      userPrice <- price
      userModel <- model
      userArea <- area
      userTimeIns <- timeKASKO
      userBenifit <- benifit
      let a = userPrice * (fst userAge) * (fst userExperience) * userModel * userArea * fst userTimeIns * userBenifit
      putStrLn "\nСтоимость КАСКО равно: "
      print a
      return (a, snd userTimeIns)
    else do
      putStrLn "Вы не могли получить водительское удостоверение раньше 18 лет, повторите попытку"
      priceKASKO

information :: IO ()
information = do
  putStrLn "\nИнформация о:\n1 - ОСАГО\n2 - КАСКО\n3 - льготах "
  info <- getLine
  case readMaybe info :: Maybe Int of
    Just 1 -> do 
        putStrLn "ОСАГО - это обязательное страхование гражданской \nответственности владельцев транспортных средств. \nОно обеспечивает компенсацию ущерба, причиненного третьим лицам \nв результате ДТП. Наша страховая компания обязуется возместить \nпострадавшему лицу расходы на лечение, восстановление \nавтомобиля и другие убытки"
        end
    Just 2 -> do 
        putStrLn "КАСКО - это добровольное страхование автомобиля \nот ущерба, который может быть причинен в результате ДТП, угона, пожара, \nстихийного бедствия и других неблагоприятных событий. Наша страховая компания \nобязуется возместить владельцу автомобиля стоимость ремонта или полную стоимость \nавтомобиля, если он будет поврежден или уничтожен"
        end
    Just 3 -> do 
        putStrLn "В нашей компании предусмотрена следующая система скидок: \n20% - инвалиды I и II группы\n30% - члены многодетной семьи\n10% - оформление страховки за 7 дней до и после дня рождения\n40% - постоянным клиентам (более 5 лет)"
        end
    _ -> do
      putStrLn "Введите только 1, 2 или 3"
      information

printForKASKO = do
    stateNumberVehicles <- validateAndSaveNumber
    file <- openFile "db.txt" ReadMode
    contents <- hGetContents file
    let insurances = map read $ lines contents :: [Insurance]
    let matchingInsurance = filter (\ins -> stateNumberVehicle ins == stateNumberVehicles && typeInsurance ins == "KASKO") insurances
    case matchingInsurance of
        [] -> do 
          costInsurance <- priceKASKO
          putStrLn "\nХотите оставить заявку на страхование и сохранить ваши данные?:\n1 - Да\n2 - Нет"
          input <- getLine
          if input == "1" then do 
            putStrLn "\nКогда вы хотите заключить договор КАСКО?" 
            startDate <- createNewDate
            fileWrite <- openFile "test.txt" AppendMode
            hPrint fileWrite $ Insurance stateNumberVehicles "KASKO" (snd costInsurance) (fst costInsurance) startDate
            hClose fileWrite
            end
          else end
        _ -> do 
          putStrLn $ "\nТС уже есть в базе страхования КАСКО, стоимость страхования равна " ++ show (priceInsurance (head matchingInsurance)) ++ "\nПовторите попытку"
          calculatorTheCostOfInsurance

printForOSAGO :: IO ()
printForOSAGO = do
    stateNumberVehicles <- validateAndSaveNumber
    file <- openFile "db.txt" ReadMode
    contents <- hGetContents file
    let insurances = map read $ lines contents :: [Insurance]
    let matchingInsurance = filter (\ins -> stateNumberVehicle ins == stateNumberVehicles && typeInsurance ins == "OSAGO") insurances
    case matchingInsurance of
        [] -> do 
          costInsurance <- priceOSAGO
          putStrLn "\nХотите оставить заявку на страхование и сохранить ваши данные?:\n1 - Да\n2 - Нет"
          input <- getLine
          if input == "1" then do
            putStrLn "\nКогда вы хотите заключить договор ОСАГО?" 
            startDate <- createNewDate
            fileWrite <- openFile "test.txt" AppendMode
            hPrint fileWrite $ Insurance stateNumberVehicles "OSAGO" (snd costInsurance) (fst costInsurance) startDate
            hClose fileWrite
            end
          else end
        _ -> do 
          putStrLn $ "\nТС уже есть в базе страхования ОСАГО, стоимость страхования равна " ++ show (priceInsurance (head matchingInsurance)) ++ "\nПовторите попытку"
          calculatorTheCostOfInsurance

typeInsuranceForRecalcculation :: IO String
typeInsuranceForRecalcculation = do
  putStrLn "\nКакое страхование необходимо рассчитать:\n1 - ОСАГО\n2 - КАСКО"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just 1 -> return "OSAGO"
    Just 2 -> return "KASKO"
    _ -> do
      putStrLn "Введите только число 1 или 2"
      typeInsuranceForRecalcculation

searchInsuranceData :: String -> String -> IO (Either String (Int, Double, Date))
searchInsuranceData stateNumber typeIns = do
    file <- openFile "db.txt" ReadMode
    contents <- hGetContents file
    let insurances = map read $ lines contents :: [Insurance]
    let matchingInsurance = filter (\ins -> stateNumberVehicle ins == stateNumber && typeInsurance ins == typeIns) insurances
    case matchingInsurance of
        [] -> return $ Left "ТС с таким гос.номером и видом страхования не найдено, повторите попытку" 
        (ins:_) -> return $ Right (timeInsurance ins, priceInsurance ins, startDate ins)

fine :: Int -> Int -> Double
fine timeIns factTime
  | fromIntegral factTime * 100 / fromIntegral timeIns >= 100 = 0
  | fromIntegral factTime * 100 / fromIntegral timeIns <= 30 = 0.9
  | fromIntegral factTime * 100 / fromIntegral timeIns > 30 && fromIntegral factTime * 100 / fromIntegral timeIns <= 50 = 0.7
  | otherwise = 0.5

recalculationService :: IO ()
recalculationService = do
    stateNumberVehicle <- validateAndSaveNumber
    typeIns <- typeInsuranceForRecalcculation
    putStrLn "\nКогда вы хотите расторгнуть договор страхования?" 
    finishDate <- createNewDate
    result <- searchInsuranceData stateNumberVehicle typeIns
    case result of
        Right (time, price, startDate) -> do 
          let compare = compareDates finishDate startDate
          if compare == GT || compare == EQ then do
            let monthsDifference = differenceInMonths finishDate startDate
            let comission = fine time monthsDifference
            let res = recalculate price time monthsDifference comission
            putStrLn $ "Возвращенная сумма равна" ++ show res
            end 
          else do 
            putStrLn "\nДата расторжения договора не может быть раньше даты страхования, повторите попытку\n"
            recalculationService
        Left errMsg -> do
          putStrLn errMsg
          recalculationService

compareDates :: Date -> Date -> Ordering
compareDates date1 date2
    | year date1 > year date2 = GT
    | year date1 < year date2 = LT
    | month date1 > month date2 = GT
    | month date1 < month date2 = LT
    | day date1 > day date2 = GT
    | day date1 < day date2 = LT
    | otherwise = EQ

recalculate :: Double -> Int -> Int -> Double -> Double
recalculate cost timeIns factTimeIns fine = (cost - (cost / fromIntegral timeIns) * fromIntegral factTimeIns) * fine

differenceInMonths :: Date -> Date -> Int
differenceInMonths date1 date2 = (year1 - year2) * 12 + (month1 - month2) + if day1 < day2 then -1 else 0
  where
    year1 = year date1
    month1 = month date1
    day1 = day date1
    year2 = year date2
    month2 = month date2
    day2 = day date2

validateAndSaveNumber :: IO String
validateAndSaveNumber = do
  putStrLn "\nВведите гос. номер ТС в формате A123BC45: "
  input <- getLine
  if isValidNumber input
    then do
      return input
    else do
      putStrLn "Некорректный номер. Повторите ввод."
      validateAndSaveNumber

isValidNumber :: String -> Bool
isValidNumber number =
  length number >= 8 && length number <= 9 &&
  isLetter (head number) &&
  all isDigit (take 3 (drop 1 number)) &&
  all isLetter (take 2 (drop 4 number)) &&
  (length (drop 6 number) == 2 || length (drop 6 number) == 3) 

isLetter :: Char -> Bool
isLetter c = c `elem` ['А'..'Я'] || c `elem` ['A'..'Z']

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

area :: IO Double
area = do
  putStrLn "\nВыберите ваш район:\n1 - Ленинский\n2 - Октябрьский\n3 - Кировский\n4 - Советский "
  areaInput <- getLine
  case readMaybe areaInput :: Maybe Int of
    Just 1 -> return 0.9
    Just 2 -> return 1
    Just 3 -> return 0.8
    Just 4 -> return 0.7
    _ -> do
      putStrLn "Введите только число 1, 2, 3 или 4"
      area

timeKASKO :: IO (Double, Int)
timeKASKO = do
  putStrLn "\nВыберите срок страхования:\n1 - 6 месяцев\n2 - 12 месяцев "
  timeKASKOInput <- getLine
  case readMaybe timeKASKOInput :: Maybe Int of
    Just 1 -> return (0.4, 6)
    Just 2 -> return (0.6, 12)
    _ -> do
      putStrLn "Введите только число 1 или 2"
      timeKASKO

model :: IO Double
model = do
  putStrLn "\nВыберете автомобиль:\n1 - Lada\n2 - Kia\n3 - Hyundai\n4 - Volkswagen\n5 - Toyota\n6 - Ford\n7 - Nissan "
  modelInput <- getLine
  case readMaybe modelInput :: Maybe Int of
    Just 1 -> return 0.9
    Just 2 -> return 0.7
    Just 3 -> return 0.65
    Just 4 -> return 0.75
    Just 5 -> return 0.6
    Just 6 -> return 0.8
    Just 7 -> return 0.85
    _ -> do
      putStrLn "Введите только число 1, 2, 3, 4, 5, 6 или 7"
      model

timeOSAGO :: IO (Double, Int)
timeOSAGO = do
  putStrLn "\nВыберите срок страхования:\n1 - 3  месяца\n2 - 6 месяцев\n3 - 12 месяцев "
  timeOSAGOInput <- getLine
  case readMaybe timeOSAGOInput :: Maybe Int of
    Just 1 -> return (1, 3)
    Just 2 -> return (0.8, 6)
    Just 3 -> return (0.6, 12)
    _ -> do
      putStrLn "Введите только число 1, 2 или 3"
      timeOSAGO   

typeAuto :: IO Double
typeAuto = do
  putStrLn "\nВыберете тип ТС:\n1 - мотоцикл\n2 - легковой автомобиль\n3 - грузовой автомобиль "
  typeAutoInput <- getLine 
  case readMaybe typeAutoInput :: Maybe Int of
    Just 1 -> return 0.5
    Just 2 -> return 0.6
    Just 3 -> return 0.7
    _ -> do
      putStrLn "Введите только число 1, 2 или 3"
      typeAuto

power :: IO Double
power = do
  putStrLn "\nВведите мощность двигателя (л.с.), только число"
  input <- getLine
  case reads input of
    [(number, "")] -> if number <=0 
            then do 
            putStrLn "Мощность может быть только положительной!"
            power
            else if number < 100
                 then return 0.5
                 else if number > 150
                      then return 1
                      else return 0.7
    _ -> do
      putStrLn "Введите только число"
      power

price :: IO Double
price = do
  putStrLn "\nВведите цену, только число:"
  input <- getLine
  case reads input of
    [(number, "")] -> if number <=0 
            then do 
            putStrLn "Стоимость может быть только положительной!"
            price
            else if number > 100000000000
                 then do 
                 putStrLn "Слишком большое число, введите реалистичную стоимость ТС"
                 price
                      else return number
    _ -> do
      putStrLn "Введите только число"
      price

age :: IO (Double, Int)
age = do
  putStrLn "\nВведите ваш возраст, только число: "
  input <- getLine
  case reads input of
    [(number, "")] -> if number < 18 || number > 120
        then do 
        putStrLn "Введите реальный возраст"
        age
        else if number < 25
                      then return (0.8, number)
                      else if number > 50
                           then return (0.7, number)
                           else return (0.6, number)
    _ -> do
      putStrLn "Введите только число"
      age

experience :: IO (Double, Int)
experience = do
  putStrLn "\nВведите водительский стаж, только число: "
  input <- getLine
  case reads input of
    [(number, "")] -> if number < 0 || number > 120
        then do 
        putStrLn "Введите реальный водительский стаж"
        experience
        else if number < 2
                      then return (0.8, number)
                      else if number > 5
                           then return (0.4, number)
                           else return (0.6, number)
    _ -> do
      putStrLn "Введите только число"
      experience

benifit :: IO Double
benifit = do
  putStrLn "\nВыберете льготу:\n1 - инвалидность\n2 - многодетная семья\n3 - день рождения\n4 - клиент компании более 5 лет\n5 - нет льгот "
  benifitInput <- getLine
  case readMaybe benifitInput :: Maybe Int of
    Just 1 -> return 0.8
    Just 2 -> return 0.7
    Just 3 -> return 0.9
    Just 4 -> return 0.6
    Just 5 -> return 1
    _ -> do
      putStrLn "Введите только число 1, 2, 3, 4 или 5"
      benifit   

end :: IO ()
end = do 
    putStrLn "\n1 - вернуться в главное меню\n2 - завершить программу"
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just 1 -> service
        Just 2 -> do putStrLn "\nПрограмма завершена!"
        _ -> do
            putStrLn "Введите только число 1 или 2"
            end

main :: IO ()
main = do
  service
