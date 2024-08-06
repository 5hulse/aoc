package com.sgh.app;

import java.io.FileNotFoundException;
import com.sgh.app.days.*;

public class App
{
    public static void main(String[] args) throws FileNotFoundException {
        try {
            Y2023D01.run();
        } catch (FileNotFoundException e) {
            throw e;
        }
    }
}
