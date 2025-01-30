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

import static net.codecadenza.eclipse.shared.Constants.DB_UNIQUE_KEY_PREFIX;

import net.codecadenza.eclipse.diagram.domain.dialog.EditOneToManyAssociationDialog;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramUpdateCommand;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.core.commands.ExecutionEvent;
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
 * Command to re-orient a one-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class OneToManyAssociationCreateCommand extends CreateElementCommand {
	private static boolean COMMAND_LOCKED;

	private String joinTableName;
	private String joinColumnName1;
	private String joinColumnName2;
	private final EObject source;
	private final EObject target;
	private DomainObject container;

	/**
	 * @param request
	 * @param source
	 * @param target
	 */
	public OneToManyAssociationCreateCommand(CreateRelationshipRequest request, EObject source, EObject target) {
		super(request);

		this.source = source;
		this.target = target;

		if (request.getContainmentFeature() == null)
			setContainmentFeature(DomainPackage.eINSTANCE.getDomainObject_Associations());

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

		if (target != null && !(target instanceof DomainObject))
			return false;

		if (getSource() == null) {
			// Link creation is in progress; source is not defined yet
			return true;
		}

		// The target may be null here but it's possible to check the constraints
		if (getContainer() == null)
			return false;

		return CodeCadenzaBaseItemSemanticEditPolicy.LinkConstraints.canCreateOneToManyAssociation_4006(getContainer(), getSource(),
				getTarget());
	}

	/**
	 * Initialize a bidirectional one-to-many association
	 * @param otm
	 * @param reverseAssociationName
	 */
	private void initBidirectionalOneToManyAssociation(OneToManyAssociation otm, String reverseAssociationName) {
		final ManyToOneAssociation mto = DomainFactory.eINSTANCE.createManyToOneAssociation();
		mto.setCascadeMerge(otm.isCascadeMerge());
		mto.setCascadePersist(otm.isCascadePersist());
		mto.setCascadeRefresh(otm.isCascadeRefresh());
		mto.setCascadeRemove(otm.isCascadeRemove());
		mto.setDomainObject(otm.getTarget());
		mto.setFetchTypeEager(otm.isFetchTypeEager());
		mto.setName(reverseAssociationName);
		mto.setOptional(false);
		mto.setOwner(false);
		mto.setUpdatable(true);
		mto.setInsertable(true);
		mto.setTarget(otm.getDomainObject());

		// If the many-to-one association references the same object it makes sense that association is optional!
		if (mto.getTarget().equals(mto.getDomainObject()))
			mto.setOptional(true);

		// Get the primary key of the referenced target table of the many-to-one association
		final PrimaryKey refPk = otm.getDomainObject().getRootParentDomainObject(false).getDatabaseTable().getPrimaryKey();

		DBTable table = mto.getDomainObject().getDatabaseTable();

		if (table == null)
			table = mto.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

		mto.setColumn(DbFactory.eINSTANCE.createDBColumn());
		mto.getColumn().setName(joinColumnName1);
		mto.getColumn().setDatabaseTable(table);
		mto.getColumn().setColumnType(refPk.getColumn().getColumnType());
		mto.getColumn().setLength(refPk.getColumn().getLength());
		mto.getColumn().setNullable(mto.isOptional());
		mto.getColumn().setPrecision(refPk.getColumn().getPrecision());
		mto.getColumn().setScale(refPk.getColumn().getScale());
		mto.getColumn().addForeignKey(refPk.getColumn(), false);

		final DomainObject targetDomainObject = otm.getTarget();

		if (targetDomainObject.getParent() != null && !targetDomainObject.equals(targetDomainObject.getRootParentDomainObject(false))
				&& mto.getDomainObject().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
			mto.getColumn().setNullable(true);

		table.getColumns().add(mto.getColumn());

		mto.getDomainObject().getAssociations().add(mto);
		mto.setReverseAssociation(otm);

		otm.setReverseAssociation(mto);

		if (!otm.getDomainObject().getAssociations().contains(otm))
			otm.getDomainObject().getAssociations().add(otm);
	}

	/**
	 * Initialize a unidirectional one-to-many association
	 * @param otm
	 * @param addUniqueKey
	 */
	private void initUniDirectionalOneToManyAssociation(OneToManyAssociation otm, boolean addUniqueKey) {
		final Project project = otm.getDomainObject().getNamespace().getProject();
		final PrimaryKey pkSource = otm.getDomainObject().getRootParentDomainObject(false).getDatabaseTable().getPrimaryKey();
		final PrimaryKey pkTarget = otm.getTarget().getRootParentDomainObject(false).getDatabaseTable().getPrimaryKey();

		final DBTable table = DbFactory.eINSTANCE.createDBTable();
		table.setName(joinTableName);
		table.setDatabase(project.getDatabase());

		otm.setTable(table);

		// As a default, an association table should be in the same catalog and/or schema as the table of the association's owner!
		table.setSchemaName(pkSource.getTable().getSchemaName());
		table.setCatalogName(pkSource.getTable().getCatalogName());

		final DBColumn col1 = DbFactory.eINSTANCE.createDBColumn();
		col1.setColumnType(pkSource.getColumn().getColumnType());
		col1.setDatabaseTable(table);
		col1.setLength(pkSource.getColumn().getLength());
		col1.setName(joinColumnName1);
		col1.setNullable(false);
		col1.setPrecision(pkSource.getColumn().getPrecision());
		col1.setScale(pkSource.getColumn().getScale());
		col1.addForeignKey(pkSource.getColumn(), false);

		final DBColumn col2 = DbFactory.eINSTANCE.createDBColumn();
		col2.setColumnType(pkTarget.getColumn().getColumnType());
		col2.setLength(pkTarget.getColumn().getLength());
		col2.setName(joinColumnName2);
		col2.setPrecision(pkTarget.getColumn().getPrecision());
		col2.setScale(pkTarget.getColumn().getScale());
		col2.setDatabaseTable(table);
		col2.setNullable(false);
		col2.addForeignKey(pkTarget.getColumn(), false);

		table.getColumns().add(col1);
		table.getColumns().add(col2);

		project.getDatabase().getDatabaseTables().add(table);

		if (addUniqueKey) {
			// Add a unique key to the association table so that each target object can only be referenced once!
			final String keyName = DB_UNIQUE_KEY_PREFIX + table.getShortTableName();

			final DBIndex uniqueKey = DbFactory.eINSTANCE.createDBIndex();
			uniqueKey.setName(keyName);
			uniqueKey.getColumns().add(col2);
			uniqueKey.setUnique(true);
			uniqueKey.setTable(table);

			table.getIndexes().add(uniqueKey);
		}

		if (!otm.getDomainObject().getAssociations().contains(otm))
			otm.getDomainObject().getAssociations().add(otm);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		COMMAND_LOCKED = true;

		OneToManyAssociation newAssoc = DomainFactory.eINSTANCE.createOneToManyAssociation();
		getContainer().getAssociations().add(newAssoc);
		newAssoc.setDomainObject(getSource());
		newAssoc.setTarget(getTarget());

		final Shell shell = Display.getCurrent().getActiveShell();
		final var dlg = new EditOneToManyAssociationDialog(shell, false, getSource(), getTarget(), newAssoc);

		if (dlg.open() != Dialog.OK) {
			COMMAND_LOCKED = false;
			throw new IllegalStateException("Operation cancelled by user!");
		}

		try {
			if (getSource().isMappedSuperClass())
				throw new IllegalStateException("Adding associations to a mapped superclass is not supported!");

			final String reverseAssociationName = dlg.getReverseAssociationName();
			joinTableName = dlg.getJoinTableName();
			joinColumnName1 = dlg.getJoinColumnName1();
			joinColumnName2 = dlg.getJoinColumnName2();

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

			if (reverseAssociationName != null) {
				// Check for duplicate names on the reverse side!
				for (final DomainAttribute att : newAssoc.getTarget().getAllAttributes())
					if (att.getName().equals(reverseAssociationName))
						throw new IllegalStateException("An attribute with the name '" + att.getName() + "' already exists!");

				for (final AbstractDomainAssociation assoc : newAssoc.getTarget().getAllAssociations())
					if (assoc.getName().equals(reverseAssociationName))
						throw new IllegalStateException("An association with the name '" + assoc.getName() + "' already exists!");
			}

			newAssoc = dlg.getAssociation();

			if (reverseAssociationName != null) {
				initBidirectionalOneToManyAssociation(newAssoc, reverseAssociationName);

				// Update the diagram completely in order to display the second association instantly!
				final var com = new CodeCadenzaDiagramUpdateCommand();
				com.execute(new ExecutionEvent());
			}
			else
				initUniDirectionalOneToManyAssociation(newAssoc, dlg.isAddUniqueKey());

			final var domainObjectService = new DomainObjectService(newAssoc.getDomainObject().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(newAssoc.getDomainObject(), false);

			if (reverseAssociationName != null)
				domainObjectService.rebuildDomainObjectSourceFiles(newAssoc.getTarget(), false);

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
