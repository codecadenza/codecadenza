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

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;

/**
 * <p>
 * Link descriptor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaLinkDescriptor extends CodeCadenzaNodeDescriptor {
	private final EObject mySource;
	private final EObject myDestination;
	private IAdaptable mySemanticAdapter;

	/**
	 * @param source
	 * @param destination
	 * @param linkElement
	 * @param linkVID
	 */
	private CodeCadenzaLinkDescriptor(EObject source, EObject destination, EObject linkElement, int linkVID) {
		super(linkElement, linkVID);

		mySource = source;
		myDestination = destination;
	}

	/**
	 * @param source
	 * @param destination
	 * @param elementType
	 * @param linkVID
	 */
	public CodeCadenzaLinkDescriptor(EObject source, EObject destination, IElementType elementType, int linkVID) {
		this(source, destination, (EObject) null, linkVID);

		final IElementType elementTypeCopy = elementType;

		mySemanticAdapter = new IAdaptable() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
			 */
			@Override
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Object getAdapter(Class adapter) {
				if (IElementType.class.equals(adapter))
					return elementTypeCopy;

				return null;
			}
		};
	}

	/**
	 * @param source
	 * @param destination
	 * @param linkElement
	 * @param elementType
	 * @param linkVID
	 */
	public CodeCadenzaLinkDescriptor(EObject source, EObject destination, EObject linkElement, IElementType elementType,
			int linkVID) {
		this(source, destination, linkElement, linkVID);

		final IElementType elementTypeCopy = elementType;

		mySemanticAdapter = new EObjectAdapter(linkElement) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter#getAdapter(java.lang.Class)
			 */
			@Override
			public Object getAdapter(Class adapter) {
				if (IElementType.class.equals(adapter))
					return elementTypeCopy;

				return super.getAdapter(adapter);
			}
		};
	}

	/**
	 * @return the source
	 */
	public EObject getSource() {
		return mySource;
	}

	/**
	 * @return the destination
	 */
	public EObject getDestination() {
		return myDestination;
	}

	/**
	 * @return the semantic adapter
	 */
	public IAdaptable getSemanticAdapter() {
		return mySemanticAdapter;
	}

}
