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
package net.codecadenza.runtime.webclient.vaadin.tree;

import com.vaadin.flow.component.icon.VaadinIcon;
import java.io.Serializable;
import java.util.UUID;

/**
 * <p>
 * When creating a Vaadin tree view, domain objects (JPA entities or DTOs) should not be added to the tree view directly. Rather,
 * instances of this class are used as intermediate objects that are mapped to respective domain objects by their ID and their
 * type. Every object is supplied with a system-generated unique ID! This is necessary as identical items must not be added to a
 * tree component!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeItem implements Serializable {
	private static final long serialVersionUID = 2239687468938806606L;

	private final String internalId;
	private String id;
	private String type;
	private String label;
	private String link;
	private VaadinIcon icon;

	/**
	 * Constructor
	 * @param type
	 * @param label
	 */
	public TreeItem(String type, String label) {
		this.label = label;
		this.type = type;

		// Every tree item must have a unique ID!
		this.internalId = UUID.randomUUID().toString();
	}

	/**
	 * Constructor
	 * @param id
	 * @param type
	 * @param label
	 */
	public TreeItem(String id, String type, String label) {
		this(type, label);

		this.id = id;
	}

	/**
	 * Constructor
	 * @param id
	 * @param type
	 * @param label
	 * @param link
	 */
	public TreeItem(String id, String type, String label, String link) {
		this(id, type, label);

		this.link = link;
	}

	/**
	 * Constructor
	 * @param id
	 * @param type
	 * @param label
	 * @param link
	 * @param icon
	 */
	public TreeItem(String id, String type, String label, String link, VaadinIcon icon) {
		this(id, type, label, link);

		this.icon = icon;
	}

	/**
	 * @return the ID
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
	 * @return the navigation target link
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

	/**
	 * @return the item's label
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @param label
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * @return the icon
	 */
	public VaadinIcon getIcon() {
		return icon;
	}

	/**
	 * @param icon
	 */
	public void setIcon(VaadinIcon icon) {
		this.icon = icon;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return the internal ID of this tree view item
	 */
	public String getInternalId() {
		return internalId;
	}

	/**
	 * @return the name of the icon that is used in the item's template
	 */
	public String getIconName() {
		if (icon == null)
			return null;

		return icon.name().toLowerCase().replace("_", "-");
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((internalId == null) ? 0 : internalId.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null)
			return false;

		if (getClass() != obj.getClass())
			return false;

		final var other = (TreeItem) obj;

		if (internalId == null) {
			if (other.internalId != null)
				return false;
		}
		else if (!internalId.equals(other.internalId))
			return false;

		return true;
	}

}
