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

import net.codecadenza.eclipse.diagram.domain.dialog.EditManyToManyAssociationDialog;
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
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
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
 * Command to create a many-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ManyToManyAssociationCreateCommand extends CreateElementCommand {
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
	public ManyToManyAssociationCreateCommand(CreateRelationshipRequest request, EObject source, EObject target) {
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

		return CodeCadenzaBaseItemSemanticEditPolicy.LinkConstraints.canCreateManyToManyAssociation_4002(getContainer(), getSource(),
				getTarget());
	}

	/**
	 * Build the reverse many-to-many association if a many-to-many association is bidirectional
	 * @param mtm
	 * @param reverseAssociationName
	 * @return a many-to-many association
	 */
	private ManyToManyAssociation buildReverseManyToManyAssociation(ManyToManyAssociation mtm, String reverseAssociationName) {
		final ManyToManyAssociation revMtm = DomainFactory.eINSTANCE.createManyToManyAssociation();
		revMtm.setCascadeMerge(mtm.isCascadeMerge());
		revMtm.setCascadePersist(mtm.isCascadePersist());
		revMtm.setCascadeRefresh(mtm.isCascadeRefresh());
		revMtm.setCascadeRemove(mtm.isCascadeRemove());
		revMtm.setDomainObject(mtm.getTarget());
		revMtm.setFetchTypeEager(mtm.isFetchTypeEager());
		revMtm.setName(reverseAssociationName);
		revMtm.setOwner(false);
		revMtm.setTarget(mtm.getDomainObject());
		revMtm.getDomainObject().getAssociations().add(revMtm);
		revMtm.setReverseAssociation(mtm);

		mtm.setReverseAssociation(revMtm);

		return revMtm;
	}

	/**
	 * Initialize a many-to-many association
	 * @param mtm
	 * @param addUniqueKey
	 * @param reverseAssociationName
	 */
	private void initManyToManyAssociation(ManyToManyAssociation mtm, boolean addUniqueKey, String reverseAssociationName) {
		DBTable targetTable = mtm.getTarget().getDatabaseTable();

		if (targetTable == null)
			targetTable = mtm.getTarget().getRootParentDomainObject(false).getDatabaseTable();

		DBTable sourceTable = mtm.getDomainObject().getDatabaseTable();

		if (sourceTable == null)
			sourceTable = mtm.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

		final PrimaryKey pkTarget = targetTable.getPrimaryKey();
		final PrimaryKey pkSource = sourceTable.getPrimaryKey();

		mtm.setTable(DbFactory.eINSTANCE.createDBTable());
		mtm.getTable().setDatabase(mtm.getDomainObject().getNamespace().getProject().getDatabase());

		// As a default, an association table should be in the same catalog and/or schema as the table of the association's owner!
		mtm.getTable().setSchemaName(sourceTable.getSchemaName());
		mtm.getTable().setCatalogName(sourceTable.getCatalogName());
		mtm.getTable().setName(joinTableName);

		if (reverseAssociationName != null)
			buildReverseManyToManyAssociation(mtm, reverseAssociationName);

		final DBColumn col1 = DbFactory.eINSTANCE.createDBColumn();
		col1.setColumnType(pkSource.getColumn().getColumnType());
		col1.setDatabaseTable(mtm.getTable());
		col1.setLength(pkSource.getColumn().getLength());
		col1.setName(joinColumnName1);
		col1.setNullable(false);
		col1.setPrecision(pkSource.getColumn().getPrecision());
		col1.setScale(pkSource.getColumn().getScale());
		col1.addForeignKey(pkSource.getColumn(), addUniqueKey);

		final DBColumn col2 = DbFactory.eINSTANCE.createDBColumn();
		col2.setColumnType(pkTarget.getColumn().getColumnType());
		col2.setLength(pkTarget.getColumn().getLength());
		col2.setPrecision(pkTarget.getColumn().getPrecision());
		col2.setScale(pkTarget.getColumn().getScale());
		col2.setDatabaseTable(mtm.getTable());
		col2.setNullable(false);
		col2.setName(joinColumnName2);
		col2.addForeignKey(pkTarget.getColumn(), addUniqueKey);

		if (mtm.getTable().equals(targetTable))
			col2.setName(targetTable.getShortTableName() + "_2pk");

		mtm.getTable().getColumns().add(col1);
		mtm.getTable().getColumns().add(col2);
		mtm.getTable().getDatabase().getDatabaseTables().add(mtm.getTable());

		if (!mtm.getDomainObject().getAssociations().contains(mtm))
			mtm.getDomainObject().getAssociations().add(mtm);

		if (addUniqueKey) {
			// Add a unique key to the association table!
			final String keyName = DB_UNIQUE_KEY_PREFIX + mtm.getTable().getShortTableName();

			final DBIndex uniqueKey = DbFactory.eINSTANCE.createDBIndex();
			uniqueKey.setName(keyName);
			uniqueKey.getColumns().add(col1);
			uniqueKey.getColumns().add(col2);
			uniqueKey.setUnique(true);
			uniqueKey.setTable(mtm.getTable());

			mtm.getTable().getIndexes().add(uniqueKey);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		COMMAND_LOCKED = true;

		final ManyToManyAssociation newAssoc = DomainFactory.eINSTANCE.createManyToManyAssociation();
		getContainer().getAssociations().add(newAssoc);
		newAssoc.setDomainObject(getSource());
		newAssoc.setTarget(getTarget());

		final Shell shell = Display.getCurrent().getActiveShell();
		final var dlg = new EditManyToManyAssociationDialog(shell, false, newAssoc);

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

			for (final AbstractDomainAssociation assoc : newAssoc.getDomainObject().getAllAssociations()) {
				if (assoc.equals(newAssoc))
					continue;

				if (assoc.getName().equals(newAssoc.getName()))
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

			initManyToManyAssociation(newAssoc, dlg.isAddUniqueKey(), reverseAssociationName);

			final var domainObjectService = new DomainObjectService(newAssoc.getDomainObject().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(newAssoc.getDomainObject(), false);

			if (reverseAssociationName != null) {
				domainObjectService.rebuildDomainObjectSourceFiles(newAssoc.getTarget(), false);

				// Update the diagram completely in order to display the second association instantly!
				final var com = new CodeCadenzaDiagramUpdateCommand();
				com.execute(new ExecutionEvent());
			}

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
