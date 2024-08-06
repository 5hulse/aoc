package com.sgh.app.days;

import java.io.FileNotFoundException;

import com.sgh.app.Utils;

import java.lang.Character;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.List;

public class Y2023D01 {
    static final int year = 2023;
    static final int day = 1;
    static final List<String> lines;

    static {
        List<String> lns = null;
        try{
            lns = Utils.getInputLines(2023, 1);
        } catch (FileNotFoundException e) {}
        lines = lns;
    }

    static final HashSet<Character> charSet = new HashSet<>(
        Arrays.asList('1', '2', '3', '4', '5', '6', '7', '8', '9')
    );

    static final HashMap<String, Character> alphaToNumMap = createAlphaToNumMap();
    private static HashMap<String, Character> createAlphaToNumMap() {
        HashMap<String, Character> map = new HashMap<>();
        map.put("one", '1');
        map.put("two", '2');
        map.put("three", '3');
        map.put("four", '4');
        map.put("five", '5');
        map.put("six", '6');
        map.put("seven", '7');
        map.put("eight", '8');
        map.put("nine", '9');
        return map;
    }

    public static void run() {
        int part1 = partOne();
        int part2 = partTwo();
        System.out.println(Utils.resultString(year, day, part1, part2));
    }

    public static int partOne() {
        int result = processList(lines);
        return result;
    }

    public static int partTwo() {
        List<String> linesCopy = new ArrayList<>(lines);
        for (int i = 0; i < linesCopy.size(); i++) {
            linesCopy.set(i, alphabetToNumerals(linesCopy.get(i)));
        }
        int result = processList(linesCopy);
        return result;
    }

    static int processList(List<String> list) {
        int sum = 0;
        String numLine;
        int lastIdx;
        for (String line : list) {
            numLine = processString(line);
            lastIdx = numLine.length() - 1;
            sum += (
                10 * Character.getNumericValue(numLine.charAt(0)) +
                Character.getNumericValue(numLine.charAt(lastIdx))
            );
        }
        return sum;
    }

    static String processString(String str) {
        String processedStr = "";
        int size = str.length();
        char c;
        for (int i=0; i<size; i++) {
            c = str.charAt(i);
            if (charSet.contains(c)) {
                processedStr += c;
            }
        }
        return processedStr;
    }

    static String alphabetToNumerals(String str) {
        String processedStr = "";
        String prefixCheck = "";
        char prefixReplacement = '0';
        int prefixLength = 0;
        boolean prefixFound;
        while (str != "") {
            prefixFound = false;
            for (Map.Entry<String, Character> entry : alphaToNumMap.entrySet()) {
                prefixCheck = entry.getKey();
                prefixReplacement = entry.getValue();
                if (str.startsWith(prefixCheck)) {
                    prefixFound = true;
                    processedStr += prefixReplacement;
                    prefixLength = prefixCheck.length();
                    str = str.substring(prefixLength - 1);
                    break;
                }
            }
            if (!prefixFound) {
                processedStr += str.charAt(0);
                str = str.substring(1);
            }
        }
        return processedStr;
    }
}
