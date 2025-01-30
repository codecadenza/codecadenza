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
package net.codecadenza.eclipse.tools.jpaeditor.service;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Result set of a JPA query
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAQueryResultSet {
	private List<String> header = new ArrayList<>();
	private List<List<String>> rows = new ArrayList<>();
	private List<Object> rowObjects = new ArrayList<>();
	private boolean flat;

	/**
	 * @return a list containing all row headers
	 */
	public List<String> getHeader() {
		return header;
	}

	/**
	 * @param header
	 */
	public void setHeader(List<String> header) {
		this.header = header;
	}

	/**
	 * @return a list that contains all objects returned by a given query
	 */
	public List<Object> getRowObjects() {
		return rowObjects;
	}

	/**
	 * @param rowObjects
	 */
	public void setRowObjects(List<Object> rowObjects) {
		this.rowObjects = rowObjects;
	}

	/**
	 * @return a list containing all rows that in turn holds a list with all cell values
	 */
	public List<List<String>> getRows() {
		return rows;
	}

	/**
	 * @param rows
	 */
	public void setRows(List<List<String>> rows) {
		this.rows = rows;
	}

	/**
	 * @return true if the structure is flat
	 */
	public boolean isFlat() {
		return flat;
	}

	/**
	 * @param flat
	 */
	public void setFlat(boolean flat) {
		this.flat = flat;
	}

}
