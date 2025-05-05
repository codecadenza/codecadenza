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
package net.codecadenza.eclipse.testing.dialog.client;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating a tree view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateTreeViewDialog extends AbstractDialog {
	private static final String LBL_DOMAIN_OBJECT = "Domain object:";
	private static final String LBL_DOMAIN_OBJECT_HIERARCHY = "Domain object hierarchy";
	private static final String LBL_TREE_VIEW_STRUCTURE = "Tree view structure";
	private static final String SHELL_TITLE = "Create new tree view";
	private static final String TREE_ITEM_ADV_SEARCH_ITEMS = "Advanced search items";
	private static final String TREE_ITEM_DISPLAY_ATTRIBUTES = "Display attributes";
	private static final String TREE_ITEM_QUICK_SEARCH_ITEMS = "Quick-search items";
	private static final String TREE_ITEM_ROOT_TREE_ITEM = "Root tree item";
	private static final String TREE_ITEM_SUB_TREE_ITEMS = "Sub-tree items";
	private static final String TREE_ITEM_TREE_NODES = "Tree nodes";
	private static final String TREE_ITEM_TREE_VIEW = "Tree view";

	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 */
	public CreateTreeViewDialog(SWTWorkbenchBot bot, DomainObject domainObject) {
		super(bot, SHELL_TITLE);

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		selectProposalItem(LBL_DOMAIN_OBJECT, domainObject.getName());

		final var domainObjectTreeView = bot.treeWithLabel(LBL_DOMAIN_OBJECT_HIERARCHY);
		final var treeViewStructure = bot.treeWithLabel(LBL_TREE_VIEW_STRUCTURE);
		final var treeStructureRootTreeItem = treeViewStructure.getTreeItem(TREE_ITEM_TREE_VIEW);
		final var quickSearchItemsTreeItem = treeStructureRootTreeItem.getNode(TREE_ITEM_QUICK_SEARCH_ITEMS);
		final var advSearchItemsTreeItem = treeStructureRootTreeItem.getNode(TREE_ITEM_ADV_SEARCH_ITEMS);
		final var displayAttributesTreeItem = treeStructureRootTreeItem.getNode(TREE_ITEM_ROOT_TREE_ITEM)
				.getNode(TREE_ITEM_DISPLAY_ATTRIBUTES);
		final var treeNodesTreeItem = treeStructureRootTreeItem.getNode(TREE_ITEM_ROOT_TREE_ITEM).getNode(TREE_ITEM_TREE_NODES);
		final var subTreeItems = treeStructureRootTreeItem.getNode(TREE_ITEM_ROOT_TREE_ITEM).getNode(TREE_ITEM_SUB_TREE_ITEMS);
		boolean displayAttributeSet = false;

		for (final var domainAttribute : domainObject.getAllAttributes()) {
			final var attrTreeItemText = "a." + domainAttribute.getName() + " (" + domainAttribute.getTypeName() + ")";
			final var attrTreeItem = domainObjectTreeView.getTreeItem(domainObject.getName()).getNode(attrTreeItemText);

			attrTreeItem.dragAndDrop(quickSearchItemsTreeItem);
			attrTreeItem.dragAndDrop(advSearchItemsTreeItem);

			if (domainAttribute.getType().equals(DomainAttribute.TYPE_STRING) && !displayAttributeSet) {
				attrTreeItem.dragAndDrop(displayAttributesTreeItem);
				displayAttributeSet = true;
			}
			else
				attrTreeItem.dragAndDrop(treeNodesTreeItem);
		}

		for (final var domainAssociation : domainObject.getAssociations()) {
			final var targetDomainObject = domainAssociation.getTarget();
			displayAttributeSet = false;

			if (domainAssociation.getType() == AssociationType.MANY_TO_ONE
					|| domainAssociation.getType() == AssociationType.ONE_TO_ONE) {
				final var assocRootTreeItemText = domainAssociation.toString();
				final var assocRootTreeItem = domainObjectTreeView.getTreeItem(domainObject.getName()).getNode(assocRootTreeItemText)
						.select().expand();

				// Add all attributes of the target domain object as tree nodes to the target tree structure
				for (final var targetAttribute : targetDomainObject.getAllAttributes()) {
					final var attrDragTreeItemName = "b." + targetAttribute.getName() + " (" + targetAttribute.getTypeName() + ")";
					final var attrTreeItem = assocRootTreeItem.getNode(attrDragTreeItemName);

					attrTreeItem.dragAndDrop(treeNodesTreeItem);
				}
			}
			else {
				// Collapse as many expanded nodes as possible in order to avoid failing drag-and-drop operations on items that aren't
				// visible in the target tree view structure!
				quickSearchItemsTreeItem.collapse();
				advSearchItemsTreeItem.collapse();
				treeNodesTreeItem.collapse();
				displayAttributesTreeItem.collapse();

				final var assocRootTreeItemText = domainAssociation.toString();
				final var assocRootTreeItem = domainObjectTreeView.getTreeItem(domainObject.getName()).getNode(assocRootTreeItemText)
						.select().expand();

				assocRootTreeItem.dragAndDrop(subTreeItems);

				final var subTreeItem = subTreeItems.getItems()[0];
				final var subItemTreeTreeItem = subTreeItem.getNode(TREE_ITEM_TREE_NODES);
				final var subItemDisplayAttributeTreeItem = subTreeItem.getNode(TREE_ITEM_DISPLAY_ATTRIBUTES);

				for (final var targetAttribute : targetDomainObject.getAllAttributes()) {
					final var attrTreeItemText = "a." + targetAttribute.getName() + " (" + targetAttribute.getTypeName() + ")";
					final var attrTreeItem = assocRootTreeItem.getNode(attrTreeItemText);

					if (targetAttribute.getType().equals(DomainAttribute.TYPE_STRING) && !displayAttributeSet) {
						attrTreeItem.dragAndDrop(subItemDisplayAttributeTreeItem);
						displayAttributeSet = true;
					}
					else
						attrTreeItem.dragAndDrop(subItemTreeTreeItem);
				}
			}
		}

		bot.button(CMD_OK).click();
	}

}
