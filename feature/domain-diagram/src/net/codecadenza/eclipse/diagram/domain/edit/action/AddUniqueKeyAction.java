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
package net.codecadenza.eclipse.diagram.domain.edit.action;

import net.codecadenza.eclipse.diagram.domain.dialog.EditIndexDialog;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorUtil;
import net.codecadenza.eclipse.model.domain.DomainObject;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;

/**
 * <p>
 * Action for creating unique keys
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AddUniqueKeyAction implements IActionDelegate {
	public static final String ID = "net.codecadenza.eclipse.diagram.domain.edit.actions.AddUniqueKeyAction";

	private DomainObject bean;
	private TransactionalEditingDomain domain;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		try {
			// This code has to be executed in a transaction, otherwise it will fail because the dialog cannot modify the attribute
			domain.getCommandStack().execute(new RecordingCommand(domain) {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
				 */
				@Override
				protected void doExecute() {
					try {
						final Shell shell = Display.getCurrent().getActiveShell();
						final var dlg = new EditIndexDialog(shell, true, bean.getDatabaseTable());

						if (dlg.open() != Window.OK)
							return;

						CodeCadenzaDiagramEditorUtil.rebuildDomainObjects(bean.getNamespace().getProject(), false, true);
					}
					catch (final Exception e) {
						CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
					}
				}
			});
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		final var sel = (StructuredSelection) selection;
		final var part = (DomainObjectEditPart) sel.getFirstElement();

		if (part != null) {
			domain = part.getEditingDomain();

			final var node = (Node) part.getModel();
			bean = (DomainObject) node.getElement();
		}
	}

}
