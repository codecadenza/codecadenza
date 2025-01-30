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
package net.codecadenza.runtime.richclient.swing.search.util;

import net.codecadenza.runtime.richclient.swing.util.type.Joiner;
import net.codecadenza.runtime.richclient.swing.util.type.Splitter;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Search operator helper
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public final class Operators {
	private Operators() {
	}

	/**
	 * Split a string containing multiple values at the proper delimiter
	 */
	public static final Splitter SPLIT_IN = Splitter.on(SearchService.TOKEN_DELIMITER_IN).trimResults();
	public static final Joiner JOIN_IN = Joiner.on(SearchService.TOKEN_DELIMITER_IN);

	/**
	 * @param op
	 * @return true if this operator allows value lists
	 */
	public static boolean allowsMultipleValues(SearchOperatorDTO op) {
		return op != null && (SearchService.OPERATOR_IN.equals(op.getValue()) || SearchService.OPERATOR_NOT_IN.equals(op.getValue()));
	}

}
