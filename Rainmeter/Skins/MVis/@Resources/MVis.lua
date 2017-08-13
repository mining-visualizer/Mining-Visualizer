
function Initialize()
	-- colorCycle is used in the context of Measure_SetBalanceColor measure.
	colorCycle = 0
end

function Update()

	if SELF:GetName() == "Measure_BlockTimeCounter" then
		-- read the value of Measure_BlockTime and display it as minutes:seconds.  after 180 seconds, turn the 
		-- display red.
		local measure = SKIN:GetMeasure('Measure_BlockTime')
		local s = measure:GetStringValue()
		if s == "0" then 
			return ""
		end
		local d = ParseDate(s)
		seconds = os.time() - d
		if seconds < 120 then
			SKIN:Bang('!SetOption', 'Meter_BlockTimeLabel', 'MeterStyle', 'BodyStyle')
			SKIN:Bang('!SetOption', 'Meter_BlockTimeValue', 'MeterStyle', 'BodyStyle')
		else
			SKIN:Bang('!SetOption', 'Meter_BlockTimeLabel', 'MeterStyle', 'BodyStyle|StyleOverdue')
			SKIN:Bang('!SetOption', 'Meter_BlockTimeValue', 'MeterStyle', 'BodyStyle|StyleOverdue')
		end
		return LeftPad(math.floor(seconds / 60), 2, "0") .. ":" .. LeftPad(seconds % 60, 2, "0")

	elseif SELF:GetName() == "Measure_SetBalanceColor" then
		-- this measure is updated externally by the data collector, and also by the Lua script as a result of a mouse click. 
		-- it is not intended to display anything, but rather to provide a trigger point so we can set the balance color.
		-- cycle the balance color through the 5 colors defined in Common.inc
		local solutionCount = SKIN:GetMeasure('Measure_SolutionCount')
		local colorIndex = (solutionCount:GetValue() + colorCycle) % 5 + 1
		SKIN:Bang('!SetOption', 'Meter_BalanceLabel', 'MeterStyle', 'BodyStyle|StyleBalanceBase|StyleBalance' .. colorIndex)
		SKIN:Bang('!SetOption', 'Meter_BalanceValue', 'MeterStyle', 'BodyStyle|StyleBalanceBase|StyleBalance' .. colorIndex)

	elseif SELF:GetName() == "Measure_HeartBeat" then
		local beat = SELF:GetOption("LastBeat")
		if (beat == '0') or ((os.time() - ParseDate(beat)) > 45) then
			SKIN:Bang('!HideMeterGroup', 'DataGroup', "MVis\\Network")
			SKIN:Bang('!HideMeterGroup', 'DataGroup', "MVis\\Miners")
			SKIN:Bang('!HideMeterGroup', 'DataGroup', "MVis\\Activity")
			SKIN:Bang('!HideMeterGroup', 'DataGroup', "MVis\\CloseHits")

			SKIN:Bang('!ShowMeterGroup', 'FlatlineGroup', "MVis\\Network")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '60', "MVis\\Network")
			SKIN:Bang('!ShowMeterGroup', 'FlatlineGroup', "MVis\\Miners")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '60', "MVis\\Miners")
			SKIN:Bang('!ShowMeterGroup', 'FlatlineGroup', "MVis\\Activity")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '60', "MVis\\Activity")
			SKIN:Bang('!ShowMeterGroup', 'FlatlineGroup', "MVis\\CloseHits")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '29R', "MVis\\CloseHits")
		else
			SKIN:Bang('!HideMeterGroup', 'FlatlineGroup', "MVis\\Network")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '0R', "MVis\\Network")
			SKIN:Bang('!HideMeterGroup', 'FlatlineGroup', "MVis\\Miners")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '0R', "MVis\\Miners")
			SKIN:Bang('!HideMeterGroup', 'FlatlineGroup', "MVis\\Activity")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '0R', "MVis\\Activity")
			SKIN:Bang('!HideMeterGroup', 'FlatlineGroup', "MVis\\CloseHits")
			SKIN:Bang('!SetOption', 'Meter_FlatLine', 'Y', '0R', "MVis\\CloseHits")

			SKIN:Bang('!ShowMeterGroup', 'DataGroup', "MVis\\Network")
			SKIN:Bang('!ShowMeterGroup', 'DataGroup', "MVis\\Miners")
			SKIN:Bang('!ShowMeterGroup', 'DataGroup', "MVis\\Activity")
			SKIN:Bang('!ShowMeterGroup', 'DataGroup', "MVis\\CloseHits")
		end
	end
end

function CycleBalanceColor()
	-- this script is called when the ETH Balance meter is clicked. it runs in the context of Measure_SetBalanceColor measure.
	if colorCycle == nil then
		colorCycle = 0
	else
		colorCycle = colorCycle + 1
	end
	SKIN:Bang('!UpdateMeasure "Measure_SetBalanceColor"')
end

 function ParseDate(s)
 	-- we're expecting YYYY-MM-DD HH:MM:SS.  hints: %d=any digits, %p=any punctuation character
	local year, month, day, hour, minute, second = s.match(s, "(%d+)%p(%d+)%p(%d+)%s+(%d+)%p(%d+)%p(%d+)")
	return os.time({day=day, month=month, year=year, hour=hour, min=minute, sec=second})
end

 function ParseDateEx(s, pattern)
	local year, month, day, hour, minute, second = s.match(s, pattern)
	return os.time({day=day, month=month, year=year, hour=hour, min=minute, sec=second})
end

function Round(num)
	return n % 1 >= 0.5 and math.ceil(n) or math.floor(n)
end

function Trunc(num)
	return math.floor(num)
end

function LeftPad(s, len, char)
	return string.rep(char, len - string.len(s)) .. s
end

