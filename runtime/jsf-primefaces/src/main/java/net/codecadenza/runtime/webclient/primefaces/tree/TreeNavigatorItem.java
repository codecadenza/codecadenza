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
package net.codecadenza.runtime.webclient.primefaces.tree;

import java.io.Serializable;

/**
 * <p>
 * Data transfer object for tree navigator items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeNavigatorItem implements Serializable {
	private static final long serialVersionUID = -2527935225276507366L;

	private String id;
	private String name;
	private String link;

	/**
	 * Constructor
	 * @param id
	 * @param label
	 * @param link
	 */
	public TreeNavigatorItem(String id, String label, String link) {
		this(label, link);
		this.id = id;
	}

	/**
	 * Constructor
	 * @param name
	 * @param link
	 */
	public TreeNavigatorItem(String name, String link) {
		this.name = name;
		this.link = link;
	}

	/**
	 * Constructor
	 * @param name
	 */
	public TreeNavigatorItem(String name) {
		this.name = name;
		this.link = null;
	}

	/**
	 * @return the ID of this item
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the item's name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the link
	 */
	public String getLink() {
		return link;
	}

	/**
	 * @param link
	 */
	public void setLink(String link) {
		this.link = link;
	}

}
