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

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;

/**
 * <p>
 * Helper class that provides static methods in order to improve handling of search fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchFieldDTOs {
	/**
	 * Prevent instantiation
	 */
	private SearchFieldDTOs() {

	}

	/**
	 * Build a deep clone of the given search field
	 * @param obj
	 * @return the cloned object
	 */
	public static SearchFieldDTO deepClone(SearchFieldDTO obj) {
		final var clone = new SearchFieldDTO();
		moveInto(obj, clone);

		final Map<String, String> enumListValues = obj.getEnumListValues();

		if (enumListValues != null)
			clone.setEnumListValues(new HashMap<>(enumListValues));

		return clone;
	}

	/**
	 * Copy search field data into another search field
	 * @param src
	 * @param dst
	 */
	private static void moveInto(SearchFieldDTO src, SearchFieldDTO dst) {
		dst.setColLabel(src.getColLabel());
		dst.setColName(src.getColName());
		dst.setColOrder(src.getColOrder());
		dst.setColumnIndex(src.getColumnIndex());
		dst.setColWidth(src.getColWidth());
		dst.setDataType(src.getDataType());
		dst.setDateTimeFormat(src.isDateTimeFormat());
		dst.setEnumListValues(src.getEnumListValues());
		dst.setFetchIndex(src.getFetchIndex());
		dst.setFilterCriteria(src.getFilterCriteria());
		dst.setListOfValues(src.getListOfValues());
		dst.setLovCommand(src.getLovCommand());
		dst.setOperator(src.getOperator());
		dst.setOriginalColumnIndex(src.getOriginalColumnIndex());
		dst.setSortIndex(src.getSortIndex());
		dst.setSortOrder(src.getSortOrder());
		dst.setType(src.getType());
		dst.setVisible(src.isVisible());
	}

}
