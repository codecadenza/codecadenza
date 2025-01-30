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

import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import org.eclipse.jface.viewers.ViewerSorter;

/**
 * <p>
 * Navigator sorter
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SuppressWarnings("deprecation")
public class CodeCadenzaNavigatorSorter extends ViewerSorter {
	private static final int GROUP_CATEGORY = 7004;
	private static final int SHORTCUTS_CATEGORY = 7003;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ViewerComparator#category(java.lang.Object)
	 */
	@Override
	public int category(Object element) {
		if (element instanceof final CodeCadenzaNavigatorItem item) {
			if (item.getView().getEAnnotation("Shortcut") != null)
				return SHORTCUTS_CATEGORY;

			return CodeCadenzaVisualIDRegistry.getVisualID(item.getView());
		}

		return GROUP_CATEGORY;
	}

}
