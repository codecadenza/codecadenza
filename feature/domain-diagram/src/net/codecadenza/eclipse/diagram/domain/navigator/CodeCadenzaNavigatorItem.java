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
package net.codecadenza.eclipse.diagram.domain.navigator;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Navigator item
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorItem extends CodeCadenzaAbstractNavigatorItem {
	static {
		final var supportedTypes = new Class[] { View.class, EObject.class };

		Platform.getAdapterManager().registerAdapters(new IAdapterFactory() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
			 */
			@Override
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Object getAdapter(Object adaptableObject, Class adapterType) {
				if (adaptableObject instanceof final CodeCadenzaNavigatorItem navigatorItem
						&& (adapterType == View.class || adapterType == EObject.class))
					return navigatorItem.getView();

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
			 */
			@Override
			public Class<?>[] getAdapterList() {
				return supportedTypes;
			}
		}, CodeCadenzaNavigatorItem.class);
	}

	private final View myView;
	private final boolean myLeaf;

	/**
	 * @param view
	 * @param parent
	 * @param isLeaf
	 */
	public CodeCadenzaNavigatorItem(View view, Object parent, boolean isLeaf) {
		super(parent);

		myView = view;
		myLeaf = isLeaf;
	}

	/**
	 * @return the view
	 */
	public View getView() {
		return myView;
	}

	/**
	 * @return true if navigator item represents a leaf
	 */
	public boolean isLeaf() {
		return myLeaf;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof final CodeCadenzaNavigatorItem navigatorItem)
			return EcoreUtil.getURI(getView()).equals(EcoreUtil.getURI(navigatorItem.getView()));

		return super.equals(obj);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return EcoreUtil.getURI(getView()).hashCode();
	}

}
