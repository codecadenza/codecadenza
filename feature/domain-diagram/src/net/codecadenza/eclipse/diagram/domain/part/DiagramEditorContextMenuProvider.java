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
import org.eclipse.emf.transaction.util.TransactionUtil;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gmf.runtime.common.ui.services.action.contributionitem.ContributionItemService;
import org.eclipse.gmf.runtime.diagram.ui.actions.ActionIds;
import org.eclipse.gmf.runtime.diagram.ui.providers.DiagramContextMenuProvider;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IWorkbenchPart;

/**
 * <p>
 * Context menu provider for the diagram editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DiagramEditorContextMenuProvider extends DiagramContextMenuProvider {
	private final IWorkbenchPart part;
	private DeleteElementAction deleteAction;

	/**
	 * @param part
	 * @param viewer
	 */
	public DiagramEditorContextMenuProvider(IWorkbenchPart part, EditPartViewer viewer) {
		super(part, viewer);

		this.part = part;

		deleteAction = new DeleteElementAction(part);
		deleteAction.init();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.MenuManager#dispose()
	 */
	@Override
	public void dispose() {
		if (deleteAction != null) {
			deleteAction.dispose();
			deleteAction = null;
		}

		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.providers.DiagramContextMenuProvider#buildContextMenu(org.eclipse.jface.action.
	 * IMenuManager)
	 */
	@Override
	public void buildContextMenu(final IMenuManager menu) {
		getViewer().flush();

		try {
			TransactionUtil.getEditingDomain((EObject) getViewer().getContents().getModel()).runExclusive(() -> {
				ContributionItemService.getInstance().contributeToPopupMenu(DiagramEditorContextMenuProvider.this, part);
				menu.remove(ActionIds.ACTION_DELETE_FROM_MODEL);
				menu.appendToGroup("editGroup", deleteAction);
			});
		}
		catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Building of context menu has been interrupted!", e);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Error building context menu", e);
		}
	}

}
