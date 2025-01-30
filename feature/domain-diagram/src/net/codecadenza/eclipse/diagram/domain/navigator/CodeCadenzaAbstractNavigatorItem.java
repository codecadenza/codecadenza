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
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertySheetPageContributor;

/**
 * <p>
 * Abstract navigator item
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class CodeCadenzaAbstractNavigatorItem extends PlatformObject {
	static {
		final var supportedTypes = new Class[] { ITabbedPropertySheetPageContributor.class };
		final ITabbedPropertySheetPageContributor propertySheetPageContributor = () -> "net.codecadenza.eclipse.diagram";

		Platform.getAdapterManager().registerAdapters(new IAdapterFactory() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
			 */
			@Override
			@SuppressWarnings({ "unchecked", "rawtypes" })
			public Object getAdapter(Object adaptableObject, Class adapterType) {
				if (adaptableObject instanceof CodeCadenzaAbstractNavigatorItem
						&& adapterType == ITabbedPropertySheetPageContributor.class)
					return propertySheetPageContributor;

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
		}, CodeCadenzaAbstractNavigatorItem.class);
	}

	private final Object myParent;

	/**
	 * @param parent
	 */
	protected CodeCadenzaAbstractNavigatorItem(Object parent) {
		myParent = parent;
	}

	/**
	 * @return the parent object
	 */
	public Object getParent() {
		return myParent;
	}

}
