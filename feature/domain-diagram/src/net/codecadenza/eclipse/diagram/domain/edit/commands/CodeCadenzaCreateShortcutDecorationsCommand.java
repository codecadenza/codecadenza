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

import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.diagram.ui.requests.CreateViewRequest;
import org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Command to create shortcut decorations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaCreateShortcutDecorationsCommand extends AbstractTransactionalCommand {
	private final List<CreateViewRequest.ViewDescriptor> myDescriptors;

	/**
	 * @param editingDomain
	 * @param parentView
	 * @param viewDescriptors
	 */
	public CodeCadenzaCreateShortcutDecorationsCommand(TransactionalEditingDomain editingDomain, View parentView,
			List<CreateViewRequest.ViewDescriptor> viewDescriptors) {
		super(editingDomain, "Create Shortcuts", getWorkspaceFiles(parentView));

		myDescriptors = viewDescriptors;
	}

	/**
	 * @param editingDomain
	 * @param parentView
	 * @param viewDescriptor
	 */
	public CodeCadenzaCreateShortcutDecorationsCommand(TransactionalEditingDomain editingDomain, View parentView,
			CreateViewRequest.ViewDescriptor viewDescriptor) {
		this(editingDomain, parentView, Collections.singletonList(viewDescriptor));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#
	 * doExecuteWithResult(org.eclipse.core.runtime.IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	protected CommandResult doExecuteWithResult(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
		myDescriptors.stream().map(descriptor -> descriptor.getAdapter(View.class)).map(View.class::cast).forEach(view -> {
			if (view != null && view.getEAnnotation("Shortcut") == null) {
				final EAnnotation shortcutAnnotation = EcoreFactory.eINSTANCE.createEAnnotation();
				shortcutAnnotation.setSource("Shortcut");
				shortcutAnnotation.getDetails().put("modelID", DomainNamespaceEditPart.MODEL_ID);

				view.getEAnnotations().add(shortcutAnnotation);
			}
		});

		return CommandResult.newOKCommandResult();
	}

}
