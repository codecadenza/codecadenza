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

import net.codecadenza.eclipse.diagram.domain.dialog.EditManyToManyAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.dialog.EditManyToOneAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.dialog.EditOneToManyAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.dialog.EditOneToOneAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;

/**
 * <p>
 * Abstract class that defines a run-method. Fields must be set by inherited classes implementing the method
 * <code>selectionChanged(IAction action, ISelection selection)</code>
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractEditDomainObjectAssociationAction implements IActionDelegate {
	protected AbstractDomainAssociation association;
	protected TransactionalEditingDomain domain;

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
					final DomainObject targetDomainObj = association.getTarget();
					final DomainObject domainObject = association.getDomainObject();
					final var domainObjectService = new DomainObjectService(domainObject.getNamespace().getProject());
					final Shell shell = Display.getCurrent().getActiveShell();

					try {
						if (association instanceof final ManyToOneAssociation mto) {
							final var d = new EditManyToOneAssociationDialog(shell, true, targetDomainObj, mto);
							final int returnCode = d.open();

							if (returnCode != Window.OK)
								return;

							association = d.getAssociation();
						}
						else if (association instanceof final ManyToManyAssociation mtm) {
							final var d = new EditManyToManyAssociationDialog(shell, true, mtm);
							final int returnCode = d.open();

							if (returnCode != Window.OK)
								return;

							association = d.getAssociation();
						}
						else if (association instanceof final OneToManyAssociation otm) {
							final var d = new EditOneToManyAssociationDialog(shell, true, domainObject, targetDomainObj, otm);
							final int returnCode = d.open();

							if (returnCode != Window.OK)
								return;

							association = d.getAssociation();
						}
						else if (association instanceof final OneToOneAssociation oto) {
							final var d = new EditOneToOneAssociationDialog(shell, true, domainObject, targetDomainObj, oto);
							final int returnCode = d.open();

							if (returnCode != Window.OK)
								return;

							association = d.getAssociation();
						}

						domainObjectService.rebuildDomainObjectSourceFiles(association.getDomainObject(), false);
						domainObjectService.rebuildDomainObjectSourceFiles(association.getTarget(), false);
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

}
