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
package net.codecadenza.eclipse.resource.dependency;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifactType;

/**
 * <p>
 * JAXB mapping class for dependency configuration objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class DependencyConfigMappingType implements Serializable {
	private static final long serialVersionUID = 1L;

	@XmlElement(name = "filter", required = false)
	private List<FilterMappingType> filters = new ArrayList<>();

	@XmlAttribute(required = true)
	private BuildArtifactType buildType;

	@XmlAttribute(required = false)
	private String comment;

	/**
	 * @return a list of elements
	 */
	public List<FilterMappingType> getFilters() {
		return this.filters;
	}

	/**
	 * @param filters
	 */
	public void setFilters(List<FilterMappingType> filters) {
		this.filters = filters;
	}

	/**
	 * @return the build type
	 */
	public BuildArtifactType getBuildType() {
		return this.buildType;
	}

	/**
	 * @param buildType
	 */
	public void setBuildType(BuildArtifactType buildType) {
		this.buildType = buildType;
	}

	/**
	 * @return the comment
	 */
	public String getComment() {
		return this.comment;
	}

	/**
	 * @param comment
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

}
