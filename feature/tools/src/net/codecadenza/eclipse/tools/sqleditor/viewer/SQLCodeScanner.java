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
package net.codecadenza.eclipse.tools.sqleditor.viewer;

import java.util.ArrayList;
import net.codecadenza.eclipse.tools.util.editor.ColorProvider;
import net.codecadenza.eclipse.tools.util.editor.WhitespaceDetector;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.swt.SWT;

/**
 * <p>
 * The scanner finds SQL comments and keywords as the user edits the document
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLCodeScanner extends RuleBasedScanner {
	/**
	 * Constructor
	 * @param syntax
	 */
	public SQLCodeScanner(SQLSyntax syntax) {
		final var provider = new ColorProvider();
		final var keyword = new Token(
				new TextAttribute(provider.getColor(ColorProvider.KEYWORD), provider.getColor(ColorProvider.BACKGROUND), SWT.BOLD));
		final var table = new Token(
				new TextAttribute(provider.getColor(ColorProvider.TABLE), provider.getColor(ColorProvider.BACKGROUND), SWT.NORMAL));
		final var string = new Token(new TextAttribute(provider.getColor(ColorProvider.STRING)));
		final var comment = new Token(new TextAttribute(provider.getColor(ColorProvider.SINGLE_LINE_COMMENT)));
		final var other = new Token(new TextAttribute(provider.getColor(ColorProvider.DEFAULT)));
		final var rules = new ArrayList<IRule>();

		setDefaultReturnToken(other);

		// Add a rule for single-line comments
		rules.add(new EndOfLineRule("//", comment));

		// Add a rule for strings and character constants
		rules.add(new SingleLineRule("\"", "\"", string, '\\'));
		rules.add(new SingleLineRule("'", "'", string, '\\'));

		// Add a generic whitespace rule
		rules.add(new WhitespaceRule(new WhitespaceDetector()));

		// Add a word rule for the reserved words and the table names
		final var wordRule = new WordRule(new SQLWordDetector(syntax), other, true);

		syntax.getReservedWords().forEach(reservedWord -> wordRule.addWord(reservedWord, keyword));
		syntax.getTableMap().keySet().forEach(tableName -> wordRule.addWord(tableName, table));

		rules.add(wordRule);

		final var result = new IRule[rules.size()];
		rules.toArray(result);

		setRules(result);
	}

}
