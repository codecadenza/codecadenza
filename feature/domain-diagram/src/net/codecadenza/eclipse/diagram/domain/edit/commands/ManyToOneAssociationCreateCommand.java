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

import net.codecadenza.eclipse.diagram.domain.dialog.EditManyToOneAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Command to create a many-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ManyToOneAssociationCreateCommand extends CreateElementCommand {
	private static boolean COMMAND_LOCKED;

	private final EObject source;
	private final EObject target;
	private DomainObject container;

	/**
	 * @param request
	 * @param source
	 * @param target
	 */
	public ManyToOneAssociationCreateCommand(CreateRelationshipRequest request, EObject source, EObject target) {
		super(request);

		this.source = source;
		this.target = target;

		if (request.getContainmentFeature() == null)
			setContainmentFeature(DomainPackage.eINSTANCE.getDomainObject_Associations());

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject) {
				container = domainObject;
				super.setElementToEdit(container);
				break;
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

		if (target != null && !(target instanceof DomainObject))
			return false;

		if (getSource() == null) {
			// Link creation is in progress; source is not defined yet
			return true;
		}

		// The target may be null here but it's possible to check the constraints
		if (getContainer() == null)
			return false;

		return CodeCadenzaBaseItemSemanticEditPolicy.LinkConstraints.canCreateManyToOneAssociation_4004(getContainer(), getSource(),
				getTarget());
	}

	/**
	 * Initialize a many-to-one association
	 * @param mto
	 */
	private void initManyToOneAssociation(ManyToOneAssociation mto) {
		DBTable table = mto.getDomainObject().getDatabaseTable();

		if (table == null)
			table = mto.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

		final PrimaryKey pk = mto.getTarget().getRootParentDomainObject(false).getDatabaseTable().getPrimaryKey();

		mto.getColumn().setColumnType(pk.getColumn().getColumnType());
		mto.getColumn().setLength(pk.getColumn().getLength());
		mto.getColumn().setPrecision(pk.getColumn().getPrecision());
		mto.getColumn().setScale(pk.getColumn().getScale());
		mto.getColumn().setNullable(mto.isOptional());
		mto.getColumn().setDatabaseTable(table);
		mto.getColumn().addForeignKey(pk.getColumn(), false);

		if (mto.getDomainObject().getParent() != null
				&& !mto.getDomainObject().equals(mto.getDomainObject().getRootParentDomainObject(false))
				&& mto.getDomainObject().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
			mto.getColumn().setNullable(true);

		table.getColumns().add(mto.getColumn());

		if (!mto.getDomainObject().getAssociations().contains(mto))
			mto.getDomainObject().getAssociations().add(mto);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		COMMAND_LOCKED = true;

		ManyToOneAssociation newAssoc = DomainFactory.eINSTANCE.createManyToOneAssociation();
		getContainer().getAssociations().add(newAssoc);
		newAssoc.setDomainObject(getSource());
		newAssoc.setTarget(getTarget());

		final Shell shell = Display.getCurrent().getActiveShell();
		final var dlg = new EditManyToOneAssociationDialog(shell, false, getTarget(), newAssoc);

		if (dlg.open() != Dialog.OK) {
			COMMAND_LOCKED = false;
			throw new IllegalStateException("Operation cancelled by user!");
		}

		try {
			if (getSource().isMappedSuperClass())
				throw new IllegalStateException("Adding associations to a mapped superclass is not supported!");

			// Check if the target provides a primary key
			if (newAssoc.getTarget().getRootParentDomainObject(true).getPKAttribute() == null)
				throw new IllegalStateException("Creating an association without a primary key is not supported!");

			// Check if the source provides a primary key
			if (newAssoc.getDomainObject().getRootParentDomainObject(true).getPKAttribute() == null)
				throw new IllegalStateException("Creating an association without a primary key is not supported!");

			// Check for duplicate names!
			for (final DomainAttribute att : newAssoc.getDomainObject().getAllAttributes())
				if (att.getName().equals(newAssoc.getName()))
					throw new IllegalStateException("An attribute with the name '" + att.getName() + "' already exists!");

			for (final AbstractDomainAssociation assoc : newAssoc.getDomainObject().getAllAssociations())
				if (assoc.getName().equals(newAssoc.getName())) {
					if (assoc.equals(newAssoc))
						continue;

					throw new IllegalStateException("An association with the name '" + assoc.getName() + "' already exists!");
				}

			newAssoc = dlg.getAssociation();

			initManyToOneAssociation(newAssoc);

			final var domainObjectService = new DomainObjectService(newAssoc.getDomainObject().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(newAssoc.getDomainObject(), false);

			COMMAND_LOCKED = false;

			return newAssoc;
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
	protected DomainObject getTarget() {
		return (DomainObject) target;
	}

	/**
	 * @return the container
	 */
	public DomainObject getContainer() {
		return container;
	}

}
