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

import net.codecadenza.eclipse.diagram.domain.dialog.CreateNewEnumAssocDialog;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.ConfigureRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateRelationshipRequest;
import org.eclipse.jface.dialogs.Dialog;

/**
 * <p>
 * Command for new enumeration associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumAssociationCreateCommand extends CreateElementCommand {
	private static boolean COMMAND_LOCKED;

	private final EObject source;
	private final EObject target;
	private DomainObject container;

	/**
	 * @param request
	 * @param source
	 * @param target
	 */
	public EnumAssociationCreateCommand(CreateRelationshipRequest request, EObject source, EObject target) {
		super(request);

		this.source = source;
		this.target = target;

		if (request.getContainmentFeature() == null)
			setContainmentFeature(DomainPackage.eINSTANCE.getDomainObject_EnumAssociations());

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null; element = element.eContainer()) {
			if (element instanceof final DomainObject domainObject) {
				container = domainObject;
				super.setElementToEdit(container);
				break;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#canExecute()
	 */
	@Override
	public boolean canExecute() {
		if (COMMAND_LOCKED)
			return false;

		if (source == null && target == null)
			return false;

		if (source != null && !(source instanceof DomainObject))
			return false;

		if (target != null && !(target instanceof JavaEnum))
			return false;

		if (getSource() == null) {
			// Link creation is in progress; source is not defined yet
			return true;
		}

		// The target may be null here but it's possible to check the constraints
		if (getContainer() == null)
			return false;

		return CodeCadenzaBaseItemSemanticEditPolicy.LinkConstraints.canCreateEnumAssociation_4001(getContainer(), getSource(),
				getTarget());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		COMMAND_LOCKED = true;

		final EnumAssociation enumAssoc = DomainFactory.eINSTANCE.createEnumAssociation();
		enumAssoc.setSource(getSource());
		enumAssoc.setTarget(getTarget());
		getContainer().getEnumAssociations().add(enumAssoc);

		final var dlg = new CreateNewEnumAssocDialog(enumAssoc);

		if (dlg.open() != Dialog.OK) {
			COMMAND_LOCKED = false;
			throw new IllegalStateException("Operation cancelled by user!");
		}

		try {
			if (getSource().isMappedSuperClass())
				throw new IllegalStateException("Adding enum associations to a mapped superclass is not supported!");

			final var domainObjectService = new DomainObjectService(enumAssoc.getSource().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(enumAssoc.getSource(), true);

			COMMAND_LOCKED = false;

			return enumAssoc;
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
			COMMAND_LOCKED = false;

			throw new IllegalStateException(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#getEClassToEdit()
	 */
	@Override
	protected EClass getEClassToEdit() {
		return DomainPackage.eINSTANCE.getDomainObject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doExecuteWithResult(org.eclipse.core.runtime.
	 * IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	protected CommandResult doExecuteWithResult(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
		if (!canExecute())
			throw new ExecutionException("Invalid arguments in the create link command!");

		return super.doExecuteWithResult(monitor, info);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#createConfigureRequest()
	 */
	@Override
	protected ConfigureRequest createConfigureRequest() {
		final ConfigureRequest request = super.createConfigureRequest();
		request.setParameter(CreateRelationshipRequest.SOURCE, getSource());
		request.setParameter(CreateRelationshipRequest.TARGET, getTarget());

		return request;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.EditElementCommand#setElementToEdit(org.eclipse.emf.ecore.EObject)
	 */
	@Override
	protected void setElementToEdit(EObject element) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @return the source domain object
	 */
	protected DomainObject getSource() {
		return (DomainObject) source;
	}

	/**
	 * @return the target domain object
	 */
	protected JavaEnum getTarget() {
		return (JavaEnum) target;
	}

	/**
	 * @return the container
	 */
	public DomainObject getContainer() {
		return container;
	}

}
