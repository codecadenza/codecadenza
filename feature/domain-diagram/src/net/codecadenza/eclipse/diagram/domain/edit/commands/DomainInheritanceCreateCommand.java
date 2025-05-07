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

import static net.codecadenza.eclipse.shared.Constants.DB_PRIMARY_KEY_PREFIX;

import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaBaseItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.ConfigureRequest;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateRelationshipRequest;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Command to create inheritance
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainInheritanceCreateCommand extends CreateElementCommand {
	private static final String DLG_TITLE = "Create domain object inheritance";

	private final EObject source;
	private final EObject target;
	private DomainObject container;

	/**
	 * @param request
	 * @param source
	 * @param target
	 */
	public DomainInheritanceCreateCommand(CreateRelationshipRequest request, EObject source, EObject target) {
		super(request);

		this.source = source;
		this.target = target;

		if (request.getContainmentFeature() == null)
			setContainmentFeature(DomainPackage.eINSTANCE.getDomainObject_Inheritance());

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
		if (source == null && target == null)
			return false;

		if (source != null && !(source instanceof DomainObject))
			return false;

		if (target != null && !(target instanceof DomainObject))
			return false;

		if (getSource() == null) {
			// Link creation is in progress, source is not defined yet
			return true;
		}

		// The target may be null here but it's possible to check the constraints
		if (getContainer() == null)
			return false;

		return CodeCadenzaBaseItemSemanticEditPolicy.LinkConstraints.canCreateDomainObjectInheritance_4005(getContainer(),
				getSource(), getTarget());
	}

	/**
	 * @param targetRoot
	 * @throws IllegalStateException if duplicate column names were found
	 */
	private void checkForDuplicateColumns(DomainObject targetRoot) {
		final Shell shell = Display.getCurrent().getActiveShell();
		final DBTable targetTable = targetRoot.getDatabaseTable();
		final DBTable sourceTable = getSource().getDatabaseTable();

		// Get all columns of the source table in order to check the column names
		for (final DBColumn col : sourceTable.getColumns())
			for (final DBColumn targetCol : targetTable.getColumns())
				if (targetCol.getConvertedName().equals(col.getConvertedName())) {
					final var message = "A column with the name '" + col.getConvertedName() + "' already exists in the superclass table!";
					MessageDialog.openInformation(shell, DLG_TITLE, message);

					throw new IllegalStateException();
				}
	}

	/**
	 * @param source
	 * @param target
	 * @throws IllegalStateException if duplicate attribute names were found
	 */
	private void checkForDuplicateAttributes(DomainObject source, DomainObject target) {
		final Shell shell = Display.getCurrent().getActiveShell();

		for (final DomainAttribute sourceAttr : source.getAttributes())
			for (final DomainAttribute targetAttr : target.getAllAttributes())
				if (sourceAttr.getName().equals(targetAttr.getName())) {
					final var message = "An attribute with the name '" + sourceAttr.getName() + "' already exists in the superclass!";
					MessageDialog.openInformation(shell, DLG_TITLE, message);

					throw new IllegalStateException();
				}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		final Shell shell = Display.getCurrent().getActiveShell();
		final DomainObject targetRoot = getTarget().getRootParentDomainObject(false);

		// Check if this domain object is already derived from another object
		if (getSource().getInheritance() != null) {
			final var message = "Only single-inheritance is supported!";
			MessageDialog.openInformation(shell, DLG_TITLE, message);

			throw new IllegalStateException();
		}

		// The base class requires an attribute equipped with a primary key!
		if (getTarget().getPKAttribute() == null) {
			final var message = "The superclass requires an ID attribute!";
			MessageDialog.openInformation(shell, DLG_TITLE, message);

			throw new IllegalStateException();
		}

		// The derived class must not provide an attribute equipped with a primary key!
		if (getSource().getPKAttribute() != null) {
			final var message = "The derived class must not provide an ID attribute!";
			MessageDialog.openInformation(shell, DLG_TITLE, message);

			throw new IllegalStateException();
		}

		// It is not allowed to create the inheritance if the generator types are different!
		if ((getTarget().isMappedSuperClass() || getTarget().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
				&& getTarget().getIDGenerator().getGeneratorType() != getSource().getIDGenerator().getGeneratorType()) {
			final var message = "The ID generator types of both classes must not be different!";
			MessageDialog.openInformation(shell, DLG_TITLE, message);

			throw new IllegalStateException();
		}

		// Search for duplicate domain attributes
		checkForDuplicateAttributes(getSource(), getTarget());

		// Test if the selected superclass supports inheritance!
		if (!getTarget().isMappedSuperClass() && targetRoot.getInheritanceType() == InheritanceTypeEnumeration.NONE) {
			final var message = "The inheritance settings in the superclass are missing!";
			MessageDialog.openInformation(shell, DLG_TITLE, message);

			throw new IllegalStateException();
		}

		if (getTarget().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE) {
			final DBTable targetTable = targetRoot.getDatabaseTable();
			final DBTable sourceTable = getSource().getDatabaseTable();

			checkForDuplicateColumns(targetRoot);

			// Get all indexes of the source table in order to check their names
			for (final DBIndex sourceIndex : sourceTable.getIndexes())
				for (final DBIndex targetIndex : targetTable.getIndexes())
					if (sourceIndex.getConvertedName().equals(targetIndex.getConvertedName())) {
						final var message = "A database index with the name '" + sourceIndex.getConvertedName()
								+ "' already exists in the superclass table!";
						MessageDialog.openInformation(shell, DLG_TITLE, message);

						throw new IllegalStateException();
					}

			// Change the database table reference of the source columns
			final var cols = new BasicEList<DBColumn>();

			sourceTable.getColumns().forEach(cols::add);

			cols.forEach(col -> col.setDatabaseTable(targetTable));

			// Change index references
			final var indexes = new BasicEList<DBIndex>();

			sourceTable.getIndexes().forEach(indexes::add);

			indexes.forEach(sourceIndex -> sourceIndex.setTable(targetTable));

			getSource().getDatabaseTable().getDatabase().getDatabaseTables().remove(getSource().getDatabaseTable());
			getSource().setDatabaseTable(null);
		}
		else if (getTarget().getInheritanceType() == InheritanceTypeEnumeration.JOINED) {
			final DomainAttribute pkAttr = getTarget().getPKAttribute();

			for (final DBColumn col : getSource().getDatabaseTable().getColumns())
				if (col.getConvertedName().equals(pkAttr.getColumn().getConvertedName())) {
					final var message = "A primary key column with the name '" + col.getConvertedName() + "' already exists!";
					MessageDialog.openInformation(shell, DLG_TITLE, message);

					throw new IllegalStateException();
				}

			final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
			col.setColumnType(pkAttr.getColumn().getColumnType());
			col.setLength(pkAttr.getColumn().getLength());
			col.setName(pkAttr.getColumn().getName());
			col.setNullable(pkAttr.getColumn().isNullable());
			col.setPrecision(pkAttr.getColumn().getPrecision());
			col.setScale(pkAttr.getColumn().getScale());
			col.setDatabaseTable(getSource().getDatabaseTable());
			col.addForeignKey(getTarget().getDatabaseTable().getPrimaryKey().getColumn(), false);

			getSource().getDatabaseTable().getColumns().add(col);

			final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
			pk.setColumn(col);
			pk.setTable(getSource().getDatabaseTable());
			pk.setName(DB_PRIMARY_KEY_PREFIX + getSource().getDatabaseTable().getShortTableName());

			getSource().getDatabaseTable().setPrimaryKey(pk);

			// In case of a JOINED inheritance strategy the primary key column of the subclass will always be filled by the JPA
			// provider!
			getSource().getIDGenerator().setGeneratorType(IDGeneratorTypeEnumeration.NONE);
		}

		final var newElement = (DomainInheritance) super.doDefaultElementCreation();
		getContainer().setInheritance(newElement);
		newElement.setSource(getSource());
		newElement.setTarget(getTarget());

		// Set the parent bean
		getSource().setParent(getTarget());

		if (getTarget().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE
				|| getTarget().getInheritanceType() == InheritanceTypeEnumeration.JOINED) {
			final var msgTitle = "Discriminator value";
			final var d = new InputDialog(shell, msgTitle, "Enter the value for the discriminator column:", "", null);

			if (d.open() != InputDialog.OK)
				throw new IllegalStateException("Operation canceled");

			if (d.getValue().isEmpty()) {
				MessageDialog.openInformation(shell, msgTitle, "A discriminator value must be provided!");

				throw new IllegalStateException("A discriminator value must be provided");
			}

			final String value = d.getValue();
			final DiscriminatorColumnTypeEnumeration type = getTarget().getDiscriminatorColumnType();

			if (type == DiscriminatorColumnTypeEnumeration.CHAR && value.length() > 1) {
				MessageDialog.openInformation(shell, msgTitle, "The discriminator value must contain exactly one character!");

				throw new IllegalStateException("The discriminator value must contain exactly one character!");
			}

			if (type == DiscriminatorColumnTypeEnumeration.INTEGER) {
				try {
					Integer.parseInt(value);
				}
				catch (final NumberFormatException e) {
					MessageDialog.openInformation(shell, msgTitle, "The discriminator value requires an integer value!");

					throw new IllegalStateException("The discriminator value requires an integer value!");
				}
			}

			getSource().setDiscriminatorColumn(getTarget().getDiscriminatorColumn());
			getSource().setDiscriminatorColumnType(getTarget().getDiscriminatorColumnType());
			getSource().setDiscriminatorValue(d.getValue());
			getSource().setInheritanceType(getTarget().getInheritanceType());
		}

		// Copy the columns to the source table and create the primary key if the domain object is derived from a mapped superclass!
		if (getTarget().isMappedSuperClass()) {
			// Before copying the columns we must check if there might be duplicate column names!
			checkForDuplicateColumns(targetRoot);

			for (final DomainAttribute att : getTarget().getAttributes()) {
				if (!att.isPersistent())
					continue;

				final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
				col.setColumnType(att.getColumn().getColumnType());
				col.setLength(att.getColumn().getLength());
				col.setName(att.getColumn().getName());
				col.setNullable(att.getColumn().isNullable());
				col.setPrecision(att.getColumn().getPrecision());
				col.setScale(att.getColumn().getScale());
				col.setDatabaseTable(getSource().getDatabaseTable());

				getSource().getDatabaseTable().getColumns().add(col);

				// Note that we only copy the primary key! Other elements like indexes and unique keys must be created manually!
				if (att.isPk()) {
					final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
					pk.setColumn(col);
					pk.setTable(getSource().getDatabaseTable());
					pk.setName(DB_PRIMARY_KEY_PREFIX + getSource().getDatabaseTable().getShortTableName());

					getSource().getDatabaseTable().setPrimaryKey(pk);
				}
			}
		}

		getTarget().getTargetInheritances().add(newElement);

		try {
			final var domainObjectService = new DomainObjectService(getSource().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(getSource(), false);
			domainObjectService.rebuildDomainObjectSourceFiles(getTarget(), false);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);

			throw new IllegalStateException(e);
		}

		return newElement;
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
