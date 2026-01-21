namespace MyLib
module Util =
  (*type Random =
    static member private generator = System.Random ()
    static member generateInt minValue maxValue = Random.generator.Next (minValue, maxValue)
  type LongitudeAndLatitude = { degree:int; minute:int; }
  type NorthLatitude = LongitudeAndLatitude
  type EastLongitude = LongitudeAndLatitude
  type EarthCrd = { north:NorthLatitude; east:EastLongitude; }
  type MapCrd = { x:int; y:int; }
  type Name = { value:string; }
  type PersonIdName = Name
  type CountryIdName = Name
  type StateIdName = Name
  type DistrictIdName = Name
  type PrefectureIdName = Name
  type PlaceIdName = Name
  type Place = { mapCrd:MapCrd; placeIdName:PlaceIdName; }*)
  type Era = | InRange of string | OutRange of string
  type WesternCalendarInputInfo = { gregorianYear:int; month:int; day:int; }
  type WesternCalendarInfo = { westernCalendarInputInfo:WesternCalendarInputInfo; totalDayFromCalendarBaseDay:int; }
  type EasternHanCalendarMonthInfo = { isLeap:bool; month:int; }
  type EasternHanCalendarInputInfo = { easternHanYear:int; easternHanMonth:EasternHanCalendarMonthInfo; dayInMonth:int; }
  type EasternHanCalendarCycleInfo = {
    year:int;
    easternHanMonth:EasternHanCalendarMonthInfo;
    daysInMonth:int;
    totalMonthFromCalendarCycleBase:int;
    totalDayFromCalendarCycleBase:int;
  }
  type EasternHanCalendarAllInfo = {
    yearInfo:{| yearFromBase:int; eraName:Era; eraYear:int option; yearSign:string; |};
    monthInfo:{| isLeap:bool; month:int; |};
    dayInfo:{| daySign:string; dayInMonth:int; |};
    totalDayFromWesternCalendarBaseDay:int;
  }
  type OutputTexts = { easternHanCalendarText:string array option; gregorianCalendarText:string array option; gregorianTotalNumberText:string array; }
  (*let normalizationEarthCrd earthCrd minuteResolution =
    let normalizationLongitudeAndLatitude crd minuteResolution =
      let slideDegree degree = (degree + 180) % 360 - 180
      let (degree, minute) = (crd.degree + crd.minute / minuteResolution, crd.minute % minuteResolution)
      let (correctionDegree, correctionMinute) = if minute < 0 then (slideDegree (degree - 1), minute + minuteResolution) else (slideDegree degree, minute)
      { crd with degree = correctionDegree; minute = correctionMinute; }
    { north = normalizationLongitudeAndLatitude earthCrd.north minuteResolution; east = normalizationLongitudeAndLatitude earthCrd.east minuteResolution; }*)
  type EasternHanCalendar =
    static member val private blockYears = 19
    static member val private blockMonths = 235
    static member val private cycleBlocks = 4
    static member val private yearDays = Fractions.Fraction 365 + Fractions.Fraction (1, 4)
    static member val private monthDays = EasternHanCalendar.yearDays * Fractions.Fraction (EasternHanCalendar.blockYears:int) / Fractions.Fraction (EasternHanCalendar.blockMonths:int)
    static member val private cycleYears = EasternHanCalendar.blockYears * EasternHanCalendar.cycleBlocks
    static member val private cycleMonths = EasternHanCalendar.blockMonths * EasternHanCalendar.cycleBlocks
    static member val private cycleDays = (EasternHanCalendar.yearDays * Fractions.Fraction (EasternHanCalendar.cycleYears:int)) |> int
    static member val private year12TermDays = EasternHanCalendar.yearDays / Fractions.Fraction 12
    static member val private baseMonth = 10
    static member val private effectiveFromStartCycle = 3
    static member val private effectiveFromEffectiveCycleDay = 6998
    static member val private effectiveFromStartDay = EasternHanCalendar.effectiveFromStartCycle * EasternHanCalendar.cycleDays + EasternHanCalendar.effectiveFromEffectiveCycleDay
    static member val private effectiveFromWesternCalendarOffset = 31460
    static member val public startFromWesternCalendarStartDayOffset = EasternHanCalendar.effectiveFromWesternCalendarOffset - EasternHanCalendar.effectiveFromStartDay
    static member val private startYearSignOffset = 15
    static member val private tenChineseStemSigns = "甲乙丙丁戊己庚辛壬癸" |> Seq.toList |> List.map string
    static member val private twelveChineseZodiacSigns = "子丑寅卯辰巳午未申酉戌亥" |> Seq.toList |> List.map string
    static member val private westernCalendarYearJackUp = 2000
    static member val public minInputWesternCalendarDayNumber = new System.DateTime(EasternHanCalendar.westernCalendarYearJackUp + 1,1,1) - new System.DateTime(1,1,1) |> _.TotalDays |> int |> fun x -> -x
    static member val public maxInputWesternCalendarDayNumber = new System.DateTime(9999,12,31) - new System.DateTime(EasternHanCalendar.westernCalendarYearJackUp + 1,1,1) |> _.TotalDays |> int
    static member val private eraCalendarData =
      lazy
      let getEraCalendarData (day, eraName) = EasternHanCalendar.buildEasternHanCalendar (EasternHanCalendar.maybeGetDayNumberFromEasternHan day) eraName (Some day.easternHanYear)
      let eraData = [
        ({ easternHanYear = 0; easternHanMonth = { isLeap = false; month = 11;}; dayInMonth = 1; }, OutRange "(四分暦実施前)");
        ({ easternHanYear = 245; easternHanMonth = { isLeap = false; month = 8;}; dayInMonth = 1; }, InRange "元和");
        ({ easternHanYear = 248; easternHanMonth = { isLeap = false; month = 7;}; dayInMonth = 1; }, InRange "章和");
        ({ easternHanYear = 250; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永元");
        ({ easternHanYear = 266; easternHanMonth = { isLeap = false; month = 4;}; dayInMonth = 1; }, InRange "元興");
        ({ easternHanYear = 267; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "延平");
        ({ easternHanYear = 268; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永初");
        ({ easternHanYear = 275; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "元初");
        ({ easternHanYear = 281; easternHanMonth = { isLeap = false; month = 4;}; dayInMonth = 1; }, InRange "永寧");
        ({ easternHanYear = 282; easternHanMonth = { isLeap = false; month = 7;}; dayInMonth = 1; }, InRange "建光");
        ({ easternHanYear = 283; easternHanMonth = { isLeap = false; month = 3;}; dayInMonth = 1; }, InRange "延光");
        ({ easternHanYear = 287; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永建");
        ({ easternHanYear = 293; easternHanMonth = { isLeap = false; month = 3;}; dayInMonth = 1; }, InRange "陽嘉");
        ({ easternHanYear = 297; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永和");
        ({ easternHanYear = 303; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "漢安");
        ({ easternHanYear = 305; easternHanMonth = { isLeap = false; month = 4;}; dayInMonth = 1; }, InRange "建康");
        ({ easternHanYear = 306; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永憙");
        ({ easternHanYear = 307; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "本初");
        ({ easternHanYear = 308; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "建和");
        ({ easternHanYear = 311; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "和平");
        ({ easternHanYear = 312; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "元嘉");
        ({ easternHanYear = 314; easternHanMonth = { isLeap = false; month = 5;}; dayInMonth = 1; }, InRange "永興");
        ({ easternHanYear = 316; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "永寿");
        ({ easternHanYear = 319; easternHanMonth = { isLeap = false; month = 6;}; dayInMonth = 1; }, InRange "延熹");
        ({ easternHanYear = 328; easternHanMonth = { isLeap = false; month = 6;}; dayInMonth = 1; }, InRange "永康");
        ({ easternHanYear = 329; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "建寧");
        ({ easternHanYear = 333; easternHanMonth = { isLeap = false; month = 5;}; dayInMonth = 1; }, InRange "熹平");
        ({ easternHanYear = 339; easternHanMonth = { isLeap = false; month = 3;}; dayInMonth = 1; }, InRange "光和");
        ({ easternHanYear = 345; easternHanMonth = { isLeap = false; month = 12;}; dayInMonth = 1; }, InRange "中平");
        ({ easternHanYear = 350; easternHanMonth = { isLeap = false; month = 4;}; dayInMonth = 13; }, InRange "光熹");
        ({ easternHanYear = 350; easternHanMonth = { isLeap = false; month = 8;}; dayInMonth = 28; }, InRange "昭寧");
        ({ easternHanYear = 350; easternHanMonth = { isLeap = false; month = 9;}; dayInMonth = 1; }, InRange "永漢");
        ({ easternHanYear = 350; easternHanMonth = { isLeap = false; month = 12;}; dayInMonth = 1; }, InRange "中平");
        ({ easternHanYear = 351; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; },  InRange "初平");
        ({ easternHanYear = 355; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "興平");
        ({ easternHanYear = 357; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "建安");
        ({ easternHanYear = 381; easternHanMonth = { isLeap = false; month = 3;}; dayInMonth = 1; }, InRange "延康");
        //↑後漢　↓蜀漢
        ({ easternHanYear = 382; easternHanMonth = { isLeap = false; month = 4;}; dayInMonth = 1; }, InRange "章武");
        ({ easternHanYear = 384; easternHanMonth = { isLeap = false; month = 5;}; dayInMonth = 1; }, InRange "建興");
        ({ easternHanYear = 399; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "延熙");
        ({ easternHanYear = 419; easternHanMonth = { isLeap = false; month = 1;}; dayInMonth = 1; }, InRange "景耀");
        ({ easternHanYear = 424; easternHanMonth = { isLeap = false; month = 8;}; dayInMonth = 1; }, InRange "炎興");
        ({ easternHanYear = 424; easternHanMonth = { isLeap = false; month = 12;}; dayInMonth = 1; }, OutRange "(四分暦廃止後)");
      ]
      List.map getEraCalendarData eraData
    static member val private calendar =
      let calcTotalMonthDays (count:int) = (EasternHanCalendar.monthDays * Fractions.Fraction count) |> int
      let calcTotalSolarTermDays (count:int) = (EasternHanCalendar.year12TermDays * Fractions.Fraction count) |> int
      let cycleTotalMonthDays = [ for i in 1 .. EasternHanCalendar.cycleMonths -> i, calcTotalMonthDays i ]
      let cycleTotalSolarTermsDays = [ for i in 1 .. 12 * EasternHanCalendar.cycleYears -> calcTotalSolarTermDays i ]
      let monthInfos =
        let cookMonthInfo nowTotalMonthDays maybePrevTotalMonthDays =
          let monthIndexFromWinterSolstice elem = List.findIndex ((<=) elem) cycleTotalSolarTermsDays
          let calcMonth totalMonthDays = (monthIndexFromWinterSolstice totalMonthDays + EasternHanCalendar.baseMonth) % 12 + 1
          let calcIsLeapMonth month = Some month = Option.map calcMonth maybePrevTotalMonthDays
          let month = calcMonth nowTotalMonthDays
          let monthDayCount = nowTotalMonthDays - Option.defaultValue 0 maybePrevTotalMonthDays
          { isLeap = calcIsLeapMonth month; month = month; } ,monthDayCount
        List.mapi (fun index totalMonthDays -> cookMonthInfo (snd totalMonthDays) (Option.map snd (List.tryItem (index - 1) cycleTotalMonthDays))) cycleTotalMonthDays
      let rec toInfo yearState monthInfos cycleTotalMonthDays =
        match monthInfos with
          | [] -> []
          | monthInfoHead::monthInfoTail ->
            let newYearState = yearState + (if monthInfoHead |> fst |> _.month = 1 && monthInfoHead |> fst |> _.isLeap |> not then 1 else 0)
            let info = {
              year = newYearState;
              easternHanMonth = {
                isLeap = monthInfoHead |> fst |> _.isLeap;
                month = monthInfoHead |> fst |> _.month;
              };
              daysInMonth = monthInfoHead |> snd
              totalMonthFromCalendarCycleBase = fst (List.head cycleTotalMonthDays);
              totalDayFromCalendarCycleBase = snd (List.head cycleTotalMonthDays);
            }
            info :: toInfo newYearState (monthInfoTail) (List.tail cycleTotalMonthDays)
      toInfo 0 monthInfos cycleTotalMonthDays
    static member private buildEasternHanCalendar maybeTotalDayFromWesternCalendarBaseDay eraName maybeEraNameYearFromBase =
      match maybeTotalDayFromWesternCalendarBaseDay with
      | None -> None
      | Some totalDayFromWesternCalendarBaseDay ->
        let totalDayFromCalendarBase = totalDayFromWesternCalendarBaseDay - EasternHanCalendar.startFromWesternCalendarStartDayOffset
        let totalDayCalendarCycleCount = totalDayFromCalendarBase / EasternHanCalendar.cycleDays
        let totalDayFromCalendarCycleBase = totalDayFromCalendarBase % EasternHanCalendar.cycleDays
        let monthIndex = (List.findIndex (fun calendar -> calendar.totalDayFromCalendarCycleBase > totalDayFromCalendarCycleBase) EasternHanCalendar.calendar)
        let maybeCurrentItem = List.tryItem monthIndex EasternHanCalendar.calendar
        let maybeBeforeItem = List.tryItem (monthIndex - 1) EasternHanCalendar.calendar
        let dayInMonth = totalDayFromCalendarCycleBase - (Option.defaultValue 0 (Option.map (_.totalDayFromCalendarCycleBase) maybeBeforeItem)) + 1
        let yearFromBase = Option.defaultValue 0 (Option.map (_.year) maybeCurrentItem) + totalDayCalendarCycleCount * EasternHanCalendar.cycleYears
        let monthIsLeap = Option.defaultValue false (Option.map (_.easternHanMonth.isLeap) maybeCurrentItem)
        let month = Option.defaultValue 0 (Option.map (_.easternHanMonth.month) maybeCurrentItem)
        let getChineseCalendarSign elem =
          let cycle60 = elem % 60
          let stemSign = List.tryItem (cycle60 % EasternHanCalendar.tenChineseStemSigns.Length) EasternHanCalendar.tenChineseStemSigns
          let zodiacSign = List.tryItem (cycle60 % EasternHanCalendar.twelveChineseZodiacSigns.Length) EasternHanCalendar.twelveChineseZodiacSigns
          Option.defaultValue "" stemSign + Option.defaultValue "" zodiacSign
        let yearSign = getChineseCalendarSign (yearFromBase + EasternHanCalendar.startYearSignOffset)
        let daySign = getChineseCalendarSign totalDayFromCalendarBase
        Some { 
          yearInfo = {| yearFromBase = yearFromBase; eraName = eraName; eraYear = Option.map ((-) (yearFromBase + 1)) maybeEraNameYearFromBase; yearSign = yearSign; |};
          monthInfo = {| isLeap = monthIsLeap; month = month; |};
          dayInfo = {| daySign = daySign; dayInMonth = dayInMonth; |};
          totalDayFromWesternCalendarBaseDay = totalDayFromWesternCalendarBaseDay;
        }
    static member public maybeGetDayNumberFromEasternHan calendarInputInfo =
      let cycleCalendarCount = calendarInputInfo.easternHanYear / EasternHanCalendar.cycleYears
      let yearInCycle = calendarInputInfo.easternHanYear % EasternHanCalendar.cycleYears
      let searchEq elem = elem.year = yearInCycle && elem.easternHanMonth.month = calendarInputInfo.easternHanMonth.month && elem.easternHanMonth.isLeap = calendarInputInfo.easternHanMonth.isLeap
      let maybeItem = List.tryFind searchEq EasternHanCalendar.calendar
      let calcPrevMonthEndDays elem = elem.totalDayFromCalendarCycleBase - elem.daysInMonth
      let maybeDayIndex = Option.map calcPrevMonthEndDays maybeItem |> Option.map ((+) (cycleCalendarCount * EasternHanCalendar.cycleDays + calendarInputInfo.dayInMonth - 1))
      Option.map ((+) EasternHanCalendar.startFromWesternCalendarStartDayOffset) maybeDayIndex
    static member private westernCalendar totalDayFromWesternCalendarBaseDay =
      let maybeDateTime = try totalDayFromWesternCalendarBaseDay |> float |> System.DateTime(EasternHanCalendar.westernCalendarYearJackUp + 1, 1, 1).AddDays |> Some with _ -> None
      match maybeDateTime with
      | None -> None
      | Some dateTime ->
        Some { 
          westernCalendarInputInfo = {
            gregorianYear = dateTime.Year - EasternHanCalendar.westernCalendarYearJackUp;
            month = dateTime.Month;
            day = dateTime.Day;
          }
          totalDayFromCalendarBaseDay = totalDayFromWesternCalendarBaseDay;
        }
    static member private easternHanCalendar totalDayFromWesternCalendarBaseDay =
      let isValidRange = EasternHanCalendar.startFromWesternCalendarStartDayOffset <= totalDayFromWesternCalendarBaseDay;
      let maybeEra = List.choose id (EasternHanCalendar.eraCalendarData.Force()) |> List.tryFindBack (fun e -> e.totalDayFromWesternCalendarBaseDay <= totalDayFromWesternCalendarBaseDay)
      let eraName = maybeEra |> Option.map (fun v -> v.yearInfo.eraName) |> Option.defaultValue (OutRange "(定義外)")
      let eraNameFromBase = maybeEra |> Option.map (fun v -> v.yearInfo.yearFromBase);
      if isValidRange then EasternHanCalendar.buildEasternHanCalendar (Some totalDayFromWesternCalendarBaseDay) eraName eraNameFromBase else None
    static member public maybeGetDayNumberFromGregorian calendarInputInfo =
      let maybeJackUpedTotalDateTime = try System.DateTime(calendarInputInfo.gregorianYear + EasternHanCalendar.westernCalendarYearJackUp, calendarInputInfo.month, calendarInputInfo.day) |> Some with _ -> None
      let maybeJackUpedTotalDays = maybeJackUpedTotalDateTime |> Option.map (fun dateTime -> dateTime - System.DateTime(1, 1, 1)) |> Option.map _.TotalDays |> Option.map int
      let jackUpDays = System.DateTime(EasternHanCalendar.westernCalendarYearJackUp + 1, 1, 1) - System.DateTime(1, 1, 1) |> _.TotalDays |> int
      Option.map ((+) -jackUpDays) maybeJackUpedTotalDays
    static member public showString calendarTotalNumber =
      let maybeEasternHanCalendarAllInfo = EasternHanCalendar.easternHanCalendar calendarTotalNumber
      let maybeGregorianCalendarAllInfo = EasternHanCalendar.westernCalendar calendarTotalNumber
      let easternHanCalendarText = 
        match maybeEasternHanCalendarAllInfo with
        | None -> None
        | Some easternHanCalendarAllInfo ->
          [
            "起点以来";
            string easternHanCalendarAllInfo.yearInfo.yearFromBase + "年";
            match easternHanCalendarAllInfo.yearInfo.eraName with | InRange a->a | OutRange a->a;
            match easternHanCalendarAllInfo.yearInfo.eraName with | InRange _-> Option.map (fun v -> string v + "年") easternHanCalendarAllInfo.yearInfo.eraYear |> Option.defaultValue "" | OutRange _->"";
            easternHanCalendarAllInfo.yearInfo.yearSign;
            (if easternHanCalendarAllInfo.monthInfo.isLeap then "閏"else "") + string easternHanCalendarAllInfo.monthInfo.month + "月";
            string easternHanCalendarAllInfo.dayInfo.dayInMonth + "日";
            easternHanCalendarAllInfo.dayInfo.daySign;
          ] |> List.toArray |> Some
      let gregorianCalendarText = 
        match maybeGregorianCalendarAllInfo with
        | None -> None
        | Some gregorianCalendarAllInfo ->
          [
            "グレゴリオ暦";
            gregorianCalendarAllInfo.westernCalendarInputInfo.gregorianYear |> fun n -> (if n > 0 then string n else "前" + string (1 - n)) + "年";
            string gregorianCalendarAllInfo.westernCalendarInputInfo.month + "月";
            string gregorianCalendarAllInfo.westernCalendarInputInfo.day + "日";
          ] |> List.toArray |> Some
      {
        easternHanCalendarText = easternHanCalendarText;
        gregorianCalendarText = gregorianCalendarText;
        gregorianTotalNumberText = [ "グレゴリオ暦経過日数"; string calendarTotalNumber; ] |> List.toArray
      }