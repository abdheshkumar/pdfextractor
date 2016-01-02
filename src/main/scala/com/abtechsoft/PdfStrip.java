package com.abtechsoft;

import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.pdfbox.util.TextPosition;

import java.io.IOException;
import java.util.List;

/**
 * Created by abdhesh on 1/1/16.
 */
public class PdfStrip extends PDFTextStripper {
    String prevBaseFont = "";

    public PdfStrip() throws IOException {
    }

    @Override
    protected void processTextPosition(TextPosition text) {

        super.processTextPosition(text);
    }

    @Override
    protected void writeString(String text, List<TextPosition> textPositions) throws IOException {
        StringBuilder builder = new StringBuilder();

        for (TextPosition position : textPositions) {
            String baseFont = position.getFont().getBaseFont();
            if (baseFont != null && !baseFont.equals(prevBaseFont)) {
                builder.append('[').append(baseFont).append(']');
                prevBaseFont = baseFont;
            }
            builder.append(position.getCharacter());
        }

        writeString(builder.toString());
    }
}
