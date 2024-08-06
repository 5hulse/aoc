package com.sgh.app;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Utils {

    public static final String inputFileTemplate = "../../inputs/Y%dD%02d.txt";
    public static final String resultStringTemplate = "year %d, day %02d, part 1: %s\nyear %d, day %02d, part 2: %s";

    public static String getInputFile(int year, int day) {
        return String.format(inputFileTemplate, year, day);
    }

    public static String resultString(int year, int day, Object part1, Object part2) {
        return String.format(resultStringTemplate, year, day, part1.toString(), year, day, part2.toString());
    }

    public static List<String> getInputLines(int year, int day) throws FileNotFoundException {
        String fileName = getInputFile(year, day);
        BufferedReader file = new BufferedReader(new FileReader(fileName));
        List<String> lines = file.lines().collect(Collectors.toList());
        return lines;
    }
}
