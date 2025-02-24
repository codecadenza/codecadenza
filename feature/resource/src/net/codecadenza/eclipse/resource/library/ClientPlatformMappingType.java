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
package net.codecadenza.eclipse.resource.library;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * JAXB mapping class for client platform types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class ClientPlatformMappingType implements Serializable {
	private static final long serialVersionUID = 7100799008205868505L;

	@XmlAttribute(required = true)
	private String name;

	@XmlElement(name = "technology", required = false)
	private List<TechnologyMappingType> technologies = new ArrayList<>();

	/**
	 * Default constructor
	 */
	public ClientPlatformMappingType() {
	}

	/**
	 * Constructor with ID attribute
	 * @param name
	 */
	public ClientPlatformMappingType(String name) {
		this.name = name;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * @param name the name to set.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return a list of elements
	 */
	public List<TechnologyMappingType> getTechnologies() {
		return this.technologies;
	}

	/**
	 * @param technologies
	 */
	public void setTechnologies(List<TechnologyMappingType> technologies) {
		this.technologies = technologies;
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

		final var mappingObject = (ClientPlatformMappingType) obj;

		if (this.name == null) {
			if (mappingObject.getName() != null)
				return false;
		}
		else if (!this.name.equals(mappingObject.getName()))
			return false;

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

}
