/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.exchange.method.imp;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Utility class for CSV data exchange method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CSVMethodGeneratorUtility {
	public static final char CHAR_TO_ESCAPE = '\'';
	public static final String DEFAULT_NULL_VALUE = "[null]";

	private final DataExchangeMethod method;

	/**
	 * Constructor
	 * @param method
	 */
	public CSVMethodGeneratorUtility(DataExchangeMethod method) {
		this.method = method;
	}

	/**
	 * @return all necessary imports
	 */
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		imports.add("import org.apache.commons.csv.*;");

		return imports;
	}

	/**
	 * @return the format generator
	 */
	public String generateFormatGenerator() {
		final var b = new StringBuilder();
		b.append("final CSVFormat format = CSVFormat.newFormat('");

		if (method.getDelimiter() == CHAR_TO_ESCAPE)
			b.append("\\");

		b.append(method.getDelimiter() + "').builder()");
		b.append(".setIgnoreEmptyLines(true)");
		b.append(".setQuoteMode(QuoteMode.ALL)");
		b.append(".setCommentMarker('");

		if (method.getCommentCharacter() == CHAR_TO_ESCAPE)
			b.append("\\");

		b.append(method.getCommentCharacter() + "')");
		b.append(".setQuote('");

		if (method.getQuoteCharacter() == CHAR_TO_ESCAPE)
			b.append("\\");

		b.append(method.getQuoteCharacter() + "')");
		b.append(".setRecordSeparator(\"" + method.getRecordSeparator() + "\")");
		b.append(".setNullString(\"[null]\").build();\n\n");

		return b.toString();
	}

}
