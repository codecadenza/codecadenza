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

import java.util.ArrayList;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;

/**
 * <p>
 * Helper class that provides static methods to improve handling of search objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchDTOs {
	/**
	 * Prevent instantiation
	 */
	private SearchDTOs() {

	}

	/**
	 * @param obj
	 * @return a deep clone of the given search object
	 */
	public static SearchDTO deepClone(SearchDTO obj) {
		final var clone = new SearchDTO();
		moveInto(obj, clone);

		if (obj.getSearchFields() != null) {
			final var fields = new ArrayList<SearchFieldDTO>();

			obj.getSearchFields().forEach(field -> fields.add(SearchFieldDTOs.deepClone(field)));

			clone.setSearchFields(fields);
		}

		return clone;
	}

	/**
	 * Copy data from one search object to another
	 * @param src
	 * @param tgt
	 */
	private static void moveInto(SearchDTO src, SearchDTO tgt) {
		tgt.setCaseSensitive(src.isCaseSensitive());
		tgt.setCount(src.isCount());
		tgt.setDateFormat(src.getDateFormat());
		tgt.setDateTimeFormat(src.getDateTimeFormat());
		tgt.setExactFilterMatch(src.isExactFilterMatch());
		tgt.setFetchHidden(src.isFetchHidden());
		tgt.setFromClause(src.getFromClause());
		tgt.setGroupBy(src.getGroupBy());
		tgt.setId(src.getId());
		tgt.setMaxResult(src.getMaxResult());
		tgt.setNumberFormat(src.getNumberFormat());
		tgt.setSearchFields(src.getSearchFields());
		tgt.setStartIndex(src.getStartIndex());
	}

}
