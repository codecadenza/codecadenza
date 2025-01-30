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
package net.codecadenza.eclipse.testing.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Objects of this class represent enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Enumeration {
	private final String name;
	private final List<String> literals = new ArrayList<>();
	private String namespace = Project.DEFAULT_NAMESPACE;
	private int startXPosition;
	private int startYPosition;

	/**
	 * Constructor
	 * @param name
	 */
	public Enumeration(String name) {
		this.name = name;
	}

	/**
	 * @return the name of the enumeration
	 */
	public String getName() {
		return name;
	}

	/**
	 * Add a literal to this enumeration
	 * @param literal
	 */
	public void addLiteral(String literal) {
		literals.add(literal);
	}

	/**
	 * @return the list with the literals
	 */
	public List<String> getLiterals() {
		return literals;
	}

	/**
	 * @param startXPosition
	 */
	public void setStartXPosition(int startXPosition) {
		this.startXPosition = startXPosition;
	}

	/**
	 * @return the namespace
	 */
	public String getNamespace() {
		return namespace;
	}

	/**
	 * @param namespace
	 */
	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	/**
	 * @return the X-position of the enumeration in the graphical editor
	 */
	public int getStartXPosition() {
		return startXPosition;
	}

	/**
	 * @param startYPosition
	 */
	public void setStartYPosition(int startYPosition) {
		this.startYPosition = startYPosition;
	}

	/**
	 * @return the Y-position of the enumeration in the graphical editor
	 */
	public int getStartYPosition() {
		return startYPosition;
	}

}
