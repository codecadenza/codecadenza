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
package net.codecadenza.eclipse.tools.util.editor;

import java.util.ArrayList;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;

/**
 * <p>
 * Document partition scanner
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PartitionScanner extends RuleBasedPartitionScanner {
	/**
	 * Create the partitioner and set up the appropriate rules
	 */
	public PartitionScanner() {
		final var literal = new Token(CommonEditorConstants.TOKEN_LITERAL);
		final var multiLineComment = new Token(CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT);
		final var singleLineComment = new Token(CommonEditorConstants.TOKEN_SINGEL_LINE_COMMENT);
		final var rules = new ArrayList<IRule>();

		// Add a rule for single-line comments
		rules.add(new EndOfLineRule("--", singleLineComment));

		// Add a rule for literals
		rules.add(new SingleLineRule("'", "'", literal, '\\'));

		// Add a rule for literals
		rules.add(new SingleLineRule("\"", "\"", literal, '\\'));

		// Add a rule for multi-line comments
		rules.add(new MultiLineRule("/*", "*/", multiLineComment));

		final var result = new IPredicateRule[rules.size()];
		rules.toArray(result);

		setPredicateRules(result);
	}

}
