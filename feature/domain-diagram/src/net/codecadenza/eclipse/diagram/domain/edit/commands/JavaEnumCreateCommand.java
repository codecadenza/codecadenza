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
package net.codecadenza.eclipse.diagram.domain.edit.commands;

import net.codecadenza.eclipse.diagram.domain.dialog.CreateNewJavaEnumDialog;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.java.JavaEnumService;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateElementRequest;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Command to create an enumeration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaEnumCreateCommand extends CreateElementCommand {
	private static final String DLG_TITLE = "Create new enumeration";

	/**
	 * @param req
	 */
	public JavaEnumCreateCommand(CreateElementRequest req) {
		super(req);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#getElementToEdit()
	 */
	@Override
	protected EObject getElementToEdit() {
		EObject container = ((CreateElementRequest) getRequest()).getContainer();

		if (container instanceof final View view)
			container = view.getElement();

		return container;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#getEClassToEdit()
	 */
	@Override
	protected EClass getEClassToEdit() {
		return DomainPackage.eINSTANCE.getDomainNamespace();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		final var javaEnum = (JavaEnum) super.doDefaultElementCreation();
		final var req = (CreateElementRequest) getRequest();
		final var namespace = (DomainNamespace) req.getContainer();
		final Project project = namespace.getProject();
		final Shell shell = Display.getCurrent().getActiveShell();

		javaEnum.setNamespace(namespace);

		final var dlg = new CreateNewJavaEnumDialog(shell, javaEnum);

		if (dlg.open() != Dialog.OK)
			throw new IllegalStateException();

		// Check if a class with same name already exists!
		for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
			for (final JavaType type : ns.getJavaTypes())
				if (!type.equals(javaEnum) && type.getName().equals(javaEnum.getName())) {
					final var msg = "A domain object or an enumeration with this name already exists!";
					MessageDialog.openInformation(shell, DLG_TITLE, msg);

					throw new IllegalStateException(msg);
				}

		try {
			namespace.getJavaTypes().add(javaEnum);

			new JavaEnumService(project).rebuildEnumerationSourceFile(javaEnum);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);

			throw new IllegalStateException(e);
		}

		return javaEnum;
	}

}
