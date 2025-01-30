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

import static net.codecadenza.eclipse.shared.Constants.LISTENER_SUFFIX;

import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;

/**
 * <p>
 * Action for renaming domain objects and enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RenameNodeAction implements IActionDelegate {
	public static final String ID = "net.codecadenza.eclipse.model.diagram.edit.actions.RenameNodeAction";
	private static final String DLG_TITLE_DOMAIN_OBJ = "Rename domain object";
	private static final String DLG_TITLE_DOMAIN_ENUM = "Rename enumeration";

	private EObject object;
	private TransactionalEditingDomain domain;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		final Shell shell = Display.getCurrent().getActiveShell();

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
						if (object instanceof final DomainObject domainObject) {
							final var dlg = new InputDialog(shell, DLG_TITLE_DOMAIN_OBJ, "Enter a new domain object name:",
									domainObject.getName(), null);

							if (dlg.open() != Window.OK)
								return;

							if (!validateUniqueName(domainObject, dlg.getValue())) {
								MessageDialog.openInformation(shell, DLG_TITLE_DOMAIN_OBJ,
										"A domain object or an enumeration with the same name already exists!");
								return;
							}

							// Validate the name
							final IStatus status = EclipseIDEService.validateJavaTypeName(dlg.getValue());

							if (status.getSeverity() > IStatus.INFO) {
								MessageDialog.openInformation(shell, DLG_TITLE_DOMAIN_OBJ, status.getMessage());
								return;
							}

							// Rename the domain object class
							EclipseIDEService.renameCompUnit(domainObject.getSourceFile(), dlg.getValue());

							// Rename the corresponding meta-data class
							EclipseIDEService.renameCompUnit(domainObject.getMetaModelSourceFile(), dlg.getValue() + "_");

							// Rename the callback listener. The rename operation will be omitted if no listener for this domain object exists!
							EclipseIDEService.renameCompUnit(domainObject.getListenerSourceFile(), dlg.getValue() + LISTENER_SUFFIX);

							domainObject.setName(dlg.getValue());
						}
						else if (object instanceof final JavaEnum javaEnum) {
							final var dlg = new InputDialog(shell, DLG_TITLE_DOMAIN_ENUM, "Enter a new enumeration name:", javaEnum.getName(),
									null);

							if (dlg.open() != Dialog.OK)
								return;

							if (!validateUniqueName(javaEnum, dlg.getValue())) {
								MessageDialog.openInformation(shell, DLG_TITLE_DOMAIN_ENUM,
										"A domain object or an enumeration with the same name already exists!");
								return;
							}

							// Validate the name
							final IStatus status = EclipseIDEService.validateJavaTypeName(dlg.getValue());

							if (status.getSeverity() > IStatus.INFO) {
								MessageDialog.openInformation(shell, DLG_TITLE_DOMAIN_ENUM, status.getMessage());
								return;
							}

							EclipseIDEService.renameCompUnit(javaEnum.getSourceFile(), dlg.getValue());

							javaEnum.setName(dlg.getValue());
						}
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

	/**
	 * @param type
	 * @param name
	 * @return false if a domain object or an enumeration with the same name already exists
	 */
	private boolean validateUniqueName(JavaType type, String name) {
		final Project project = type.getNamespace().getProject();

		// Test if either a domain object or an enumeration with the same name already exists
		for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
			for (final JavaType t : ns.getJavaTypes()) {
				if (type.equals(t))
					continue;

				if (t.getName().equals(name))
					return false;
			}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		final var sel = (StructuredSelection) selection;
		final var part = (ShapeNodeEditPart) sel.getFirstElement();

		if (part == null)
			return;

		domain = part.getEditingDomain();

		final var view = (View) part.getModel();
		object = view.getElement();
	}

}
