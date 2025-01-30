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

import com.vaadin.flow.component.treegrid.TreeGrid;
import com.vaadin.flow.data.renderer.LitRenderer;
import java.util.List;

/**
 * <p>
 * Utility class for adding a column with an internal default renderer to a {@link TreeGrid}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeGridColumnUtil {
	private static final String PROPERTY_LEAF = "leaf";
	private static final String PROPERTY_ICON = "icon";
	private static final String PROPERTY_TITLE = "title";
	private static final String PROPERTY_NAME = "name";
	private static final String FUNCTION_ON_CLICK = "onClick";
	private static final String TEMPLATE = "<vaadin-grid-tree-toggle title='${item.title}' "
			+ "@click=${onClick} .leaf='${item.leaf}' .expanded='${model.expanded}' .level='${model.level}'>"
			+ "<vaadin-icon icon='vaadin:${item.icon}' style='height: var(--lumo-icon-size-s);"
			+ "margin-inline-end: var(--lumo-space-s); width: var(--lumo-icon-size-s);'>"
			+ "</vaadin-icon>${item.name}</vaadin-grid-tree-toggle>";

	/**
	 * Prevent instantiation
	 */
	private TreeGridColumnUtil() {

	}

	/**
	 * Add a column with a default renderer to the given tree
	 * @param tree the {@link TreeGrid} to add the column to
	 */
	public static final void addColumn(TreeGrid<TreeItem> tree) {
		final var renderer = LitRenderer.<TreeItem> of(TEMPLATE)
				.withProperty(PROPERTY_LEAF, item -> !tree.getDataCommunicator().hasChildren(item))
				.withProperty(PROPERTY_TITLE, TreeItem::getId).withProperty(PROPERTY_ICON, TreeItem::getIconName)
				.withProperty(PROPERTY_NAME, TreeItem::getLabel).withFunction(FUNCTION_ON_CLICK, item -> {
					if (tree.getDataCommunicator().hasChildren(item)) {
						if (tree.isExpanded(item))
							tree.collapse(List.of(item));
						else
							tree.expand(List.of(item));
					}
				});

		tree.addColumn(renderer);
	}

}
