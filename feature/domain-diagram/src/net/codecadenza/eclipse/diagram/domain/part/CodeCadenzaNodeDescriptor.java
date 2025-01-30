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
package net.codecadenza.eclipse.diagram.domain.part;

import org.eclipse.emf.ecore.EObject;

/**
 * <p>
 * Node descriptor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNodeDescriptor {
	private final EObject myModelElement;
	private final int myVisualID;
	private String myType;

	/**
	 * @param modelElement
	 * @param visualID
	 */
	public CodeCadenzaNodeDescriptor(EObject modelElement, int visualID) {
		myModelElement = modelElement;
		myVisualID = visualID;
	}

	/**
	 * @return the model element
	 */
	public EObject getModelElement() {
		return myModelElement;
	}

	/**
	 * @return the visual ID
	 */
	public int getVisualID() {
		return myVisualID;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		if (myType == null)
			myType = CodeCadenzaVisualIDRegistry.getType(getVisualID());

		return myType;
	}

}
