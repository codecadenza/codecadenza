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

import net.codecadenza.eclipse.diagram.domain.dialog.CreateDomainObjectDialog;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateElementRequest;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Command to create a domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectCreateCommand extends CreateElementCommand {
	/**
	 * @param req
	 */
	public DomainObjectCreateCommand(CreateElementRequest req) {
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
		final var domainObject = (DomainObject) super.doDefaultElementCreation();
		final Shell shell = Display.getCurrent().getActiveShell();
		final var req = (CreateElementRequest) getRequest();
		final var domainNamespace = (DomainNamespace) req.getContainer();
		final var dlg = new CreateDomainObjectDialog(shell, domainObject, domainNamespace);

		if (dlg.open() != Dialog.OK)
			throw new IllegalStateException("Operation canceled by user!");

		// Build the domain object source files
		try {
			final var domainObjectService = new DomainObjectService(domainObject.getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(domainObject, true);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);

			throw new IllegalStateException(e);
		}

		return domainObject;
	}

}
