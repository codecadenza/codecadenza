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
package net.codecadenza.eclipse.tools.reverse.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAssociation;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAttribute;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainObject;
import net.codecadenza.eclipse.tools.reverse.model.RevEngEnum;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringConfig;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringLogEntry;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringLogEntry.Status;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

/**
 * <p>
 * Service for inferring the domain model from a given database
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringService {
	private static final String TABLE_SUFFIX_LONG = "table";
	private static final String TABLE_SUFFIX_SHORT = "tab";
	private static final String SEQ_SUFFIX = "_SEQ";
	private static final String REVERSE_NAME_SUFFIX = "reverse";
	private static final int SEQ_BLOCK_SIZE = 20;
	private static final int SEQ_START_INDEX = 1;

	private final Project project;
	private final Database sourceDBModel;
	private final List<DBTable> manyToManyTables = new ArrayList<>();
	private final List<ReverseEngineeringLogEntry> logEntries = new ArrayList<>();
	private final Namespace defaultNamespace;
	private ReverseEngineeringModel revEngModel = new ReverseEngineeringModel();
	private Set<String> sequences;
	private ReverseEngineeringConfig configuration;

	/**
	 * Constructor
	 * @param defaultNamespace
	 * @param sourceDBModel
	 */
	public ReverseEngineeringService(Namespace defaultNamespace, Database sourceDBModel) {
		this.project = defaultNamespace.getProject();
		this.sourceDBModel = sourceDBModel;
		this.defaultNamespace = defaultNamespace;
		this.revEngModel = new ReverseEngineeringModel(defaultNamespace);
	}

	/**
	 * @param sequences
	 */
	public void setSequences(Set<String> sequences) {
		this.sequences = sequences;
	}

	/**
	 * @param configuration
	 */
	public void setConfiguration(ReverseEngineeringConfig configuration) {
		this.configuration = configuration;
	}

	/**
	 * Perform the reverse engineering and create the initial model
	 * @return the initial reverse engineering model
	 */
	public ReverseEngineeringModel performReverseEngineering() {
		// Search for database tables and try to infer a valid domain model!
		for (final DBTable table : searchTables()) {
			// Check if the existing model already contains a table with the same name
			final boolean tableFound = project.getDatabase().getDatabaseTables().stream().anyMatch(
					existingTable -> existingTable.getName() != null && existingTable.getConvertedName().equals(table.getConvertedName()));

			// Check if the table represents an association table for a many-to-many association
			if (isManyToManyAssoc(table))
				continue;

			if (table.getPrimaryKey() == null) {
				addLog("The table '" + table.getName() + "' has either no or an invalid primary key!", Status.ERROR);
				continue;
			}
			else if (table.getPrimaryKey().getColumn() == null) {
				addLog("A column for the primary key of table '" + table.getName() + "' could not be found!", Status.ERROR);
				continue;
			}

			if (!initColumnTypes(table))
				continue;

			if (!tableFound)
				createDomainObject(table);
			else
				editDomainObject(table);
		}

		// Initialize all many-to-one association targets
		initManyToOneAssociationTargets();

		// Initialize all many-to-many associations
		initManyToManyAssociations();

		return revEngModel;
	}

	/**
	 * Save the domain model
	 * @param revEngModel
	 * @return a list containing validation log entries. If the list is empty the save operation has been finished successfully!
	 */
	public List<ReverseEngineeringLogEntry> saveDomainModel(ReverseEngineeringModel revEngModel) {
		this.revEngModel = revEngModel;

		for (final RevEngDomainObject obj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = obj.getDomainObject();

			// If the object has been created by the reverse engineering process the database table must be replaced by a copy!
			if (obj.isCreatedByReverseEngineering()) {
				final DBTable targetTable = domainObject.getDatabaseTable().copyTableToDatabase(project.getDatabase());

				domainObject.setDatabaseTable(targetTable);
			}

			// Add the attributes to the respective domain object
			for (final RevEngDomainAttribute attrObj : obj.getAttributes()) {
				if (!attrObj.isCreatedByReverseEngineering())
					continue;

				final DomainAttribute attr = attrObj.getDomainAttribute();
				attr.setDomainObject(domainObject);

				domainObject.getAttributes().add(attr);
			}

			// Add the associations to the respective domain object
			for (final RevEngDomainAssociation assocObj : obj.getAssociations()) {
				if (!assocObj.isCreatedByReverseEngineering())
					continue;

				final AbstractDomainAssociation assoc = assocObj.getAssociation();
				assoc.setDomainObject(domainObject);

				domainObject.getAssociations().add(assoc);

				if (assocObj.isManyToMany()) {
					final var mtm = (ManyToManyAssociation) assoc;

					if (mtm.getTable() != null && mtm.isOwner())
						mtm.setTable(mtm.getTable().copyTableToDatabase(project.getDatabase()));
				}
			}
		}

		// Determine all foreign key reference columns that couldn't be found while copying the tables
		for (final DBTable dbTable : project.getDatabase().getDatabaseTables())
			for (final ForeignKey targetForeignKey : dbTable.getForeignKeys()) {
				if (targetForeignKey.getReferencedColumn() != null)
					continue;

				for (final DBTable sourceTable : sourceDBModel.getDatabaseTables())
					for (final ForeignKey sourceForeignKey : sourceTable.getForeignKeys()) {
						if (!targetForeignKey.getConvertedName().equals(sourceForeignKey.getConvertedName()))
							continue;

						DBTable refTable = null;

						if (sourceForeignKey.getReferencedColumn() != null)
							refTable = sourceForeignKey.getReferencedColumn().getDatabaseTable();

						if (refTable == null)
							continue;

						for (final DBTable table : project.getDatabase().getDatabaseTables())
							if (table.getConvertedName().equals(refTable.getConvertedName())) {
								targetForeignKey.setReferencedColumn(table.getPrimaryKey().getColumn());
								break;
							}
					}
			}

		// Change the columns of attributes and associations of domain objects created by the reverse engineering process
		for (final RevEngDomainObject revEngObject : revEngModel.getDomainObjects()) {
			if (!revEngObject.isCreatedByReverseEngineering())
				continue;

			final DomainObject domainObject = revEngObject.getDomainObject();
			final DBTable dbTable = domainObject.getDatabaseTable();

			domainObject.getAttributes()
					.forEach(attr -> attr.setColumn(dbTable.getColumnByConvertedName(attr.getColumn().getConvertedName())));

			domainObject.getAssociations().forEach(assoc -> {
				if (assoc instanceof final ManyToOneAssociation mto)
					mto.setColumn(dbTable.getColumnByConvertedName(mto.getColumn().getConvertedName()));
				else if (assoc instanceof final OneToOneAssociation oto && oto.isOwner() && oto.getColumn() != null)
					oto.setColumn(dbTable.getColumnByConvertedName(oto.getColumn().getConvertedName()));
			});
		}

		// Add new database objects for new domain attributes and associations of existing domain objects
		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = revEngObj.getDomainObject();
			final DBTable dbTable = domainObject.getDatabaseTable();

			if (revEngObj.isCreatedByReverseEngineering() || dbTable == null)
				continue;

			// Add new columns to the domain attributes
			for (final RevEngDomainAttribute revEngAttr : revEngObj.getAttributes()) {
				if (!revEngAttr.isCreatedByReverseEngineering())
					continue;

				final DomainAttribute attr = revEngAttr.getDomainAttribute();
				final DBColumn newCol = dbTable.addColumnCopy(attr.getColumn());

				attr.setColumn(newCol);
			}

			// Add columns for new many-to-one and one-to-one associations
			for (final RevEngDomainAssociation revEngAssoc : revEngObj.getAssociations()) {
				if (!revEngAssoc.isOneToOne() && !revEngAssoc.isManyToOne())
					continue;

				if (!revEngAssoc.isCreatedByReverseEngineering())
					continue;

				final AbstractDomainAssociation newAssoc = revEngAssoc.getAssociation();

				if (revEngAssoc.isManyToOne()) {
					final var mto = (ManyToOneAssociation) newAssoc;

					if (mto.getColumn() == null)
						continue;

					final DBColumn newCol = dbTable.addColumnCopy(mto.getColumn());

					mto.setColumn(newCol);
				}
				else if (revEngAssoc.isOneToOne()) {
					final var oto = (OneToOneAssociation) newAssoc;

					if (oto.getColumn() == null)
						continue;

					final DBColumn newCol = dbTable.addColumnCopy(oto.getColumn());

					oto.setColumn(newCol);
				}
			}
		}

		// Search for foreign keys and indexes that should be added to existing tables
		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = revEngObj.getDomainObject();
			final DBTable existingTable = domainObject.getDatabaseTable();

			if (revEngObj.isCreatedByReverseEngineering() || existingTable == null)
				continue;

			// We must not change a table of a mapped superclass as it only represents a template!
			if (revEngObj.getDomainObject().isMappedSuperClass())
				continue;

			final DBTable sourceTable = getSourceTable(existingTable);

			if (sourceTable == null)
				continue;

			existingTable.addForeignKeyCopies(sourceTable);
			existingTable.addIndexCopies(sourceTable);
		}

		// Validate the domain model
		final var validationService = new DomainModelValidationService(revEngModel);
		final List<ReverseEngineeringLogEntry> validationEntries = validationService.validateDomainModel();

		// Don't save the domain model if the validation has failed!
		if (!validationEntries.isEmpty())
			return validationEntries;

		final TransactionalEditingDomain domain = TransactionalEditingDomain.Factory.INSTANCE
				.createEditingDomain(project.eResource().getResourceSet());

		domain.getCommandStack().execute(new RecordingCommand(domain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				// Add the domain objects to the namespaces they belong to
				for (final RevEngDomainObject revEngObject : revEngModel.getDomainObjects()) {
					if (!revEngObject.isCreatedByReverseEngineering())
						continue;

					final DomainObject domainObject = revEngObject.getDomainObject();

					for (final Namespace existingNamespace : project.getDomainNamespace().getChildNamespaces())
						if (existingNamespace.getName().equals(revEngObject.getNamespaceName())) {
							final var namespace = (DomainNamespace) existingNamespace;
							domainObject.setNamespace(namespace);

							namespace.getDomainObjects().add(domainObject);
							break;
						}
				}

				// Add the enumerations to the namespaces they belong to
				for (final RevEngEnum revEngEnum : revEngModel.getEnumerations()) {
					if (!revEngEnum.isCreatedByReverseEngineering())
						continue;

					final JavaEnum javaEnum = revEngEnum.getJavaEnum();

					for (final Namespace existingNamespace : project.getDomainNamespace().getChildNamespaces())
						if (existingNamespace.getName().equals(revEngEnum.getNamespaceName())) {
							final var namespace = (DomainNamespace) existingNamespace;
							javaEnum.setNamespace(namespace);

							namespace.getEnumerations().add(javaEnum);
							break;
						}
				}

				// Save the domain model
				try {
					for (final org.eclipse.emf.ecore.resource.Resource resource : project.eResource().getResourceSet().getResources())
						EclipseIDEService.saveProjectMetaData(resource, project);
				}
				catch (final Exception e) {
					throw new OperationCanceledException(e.getMessage());
				}
			}
		});

		return new ArrayList<>();
	}

	/**
	 * @return the reverse engineering log entries
	 */
	public List<ReverseEngineeringLogEntry> getLogEntries() {
		return logEntries;
	}

	/**
	 * @return a list of database tables that match the filter criteria
	 */
	private HashSet<DBTable> searchTables() {
		final var tableSet = new HashSet<DBTable>();
		final var tableResultSet = new HashSet<DBTable>();

		// Iterate over all tables of the source database
		for (final DBTable table : sourceDBModel.getDatabaseTables()) {
			// Check if the table name matches the filter
			if (!configuration.inputMatchesFilter(table.getConvertedName()))
				continue;

			tableSet.add(table);
		}

		// If the deep search flag is enabled we will search for further tables by analyzing the foreign keys
		if (configuration.isDeepSearchEnabled())
			tableSet.forEach(table -> searchTables(table, tableResultSet));
		else
			return tableSet;

		return tableResultSet;
	}

	/**
	 * Add the given table and all tables that are referenced via foreign keys to the result set
	 * @param table
	 * @param tableResultSet
	 */
	private void searchTables(DBTable table, HashSet<DBTable> tableResultSet) {
		tableResultSet.add(table);

		for (final ForeignKey foreignKey : table.getForeignKeys()) {
			if (foreignKey.getReferencedColumn() == null || foreignKey.getReferencedColumn().getDatabaseTable() == null)
				continue;

			final DBTable refTable = foreignKey.getReferencedColumn().getDatabaseTable();

			if (tableResultSet.contains(refTable))
				continue;

			searchTables(refTable, tableResultSet);
		}
	}

	/**
	 * Validate and initialize the database column types for all columns of a database table
	 * @param table
	 * @return true if the initialization finished successfully
	 */
	private boolean initColumnTypes(DBTable table) {
		for (final DBColumn column : table.getColumns()) {
			final String colTypeName = column.getColumnType().getName();
			final var fullColName = table.getName() + "." + column.getName();
			boolean typeExists = false;

			// The database meta-data import provides a column type for every column without checking if the type is even supported!
			for (final DBColumnType colType : project.getDatabase().getAllSupportedColumnTypes())
				if (colTypeName.equalsIgnoreCase(colType.getName())) {
					typeExists = true;
					column.setColumnType(colType);

					// If the type is configured to omit the column size information the column's length must be set to 0!
					if (colType.isOmitSizeInformation())
						column.setLength(0);

					break;
				}

			if (!typeExists) {
				addLog("The type '" + colTypeName + "' of column '" + fullColName + "' is unknown!", Status.ERROR);
				return false;
			}
		}

		return true;
	}

	/**
	 * @param domainObject
	 * @param primaryKey
	 * @return the ID generator for a given domain object
	 */
	private IDGenerator initIDGenerator(DomainObject domainObject, PrimaryKey primaryKey) {
		final IDGenerator generator = DomainFactory.eINSTANCE.createIDGenerator();
		final DBColumn pkColumn = primaryKey.getColumn();
		final JavaType pkType = getJavaTypeForColumn(pkColumn);
		final String tableName = primaryKey.getTable().getName();

		if (pkType == null)
			return null;

		generator.setName(DBNamingUtil.convertToDatabase(domainObject.getName() + SEQ_SUFFIX, project.getDatabase()));
		generator.setGeneratorType(null);

		if (pkType.isString())
			generator.setGeneratorType(IDGeneratorTypeEnumeration.NONE);
		else if (pkType.isIntegerOrLong())
			generator.setGeneratorType(IDGeneratorTypeEnumeration.IDENTITY);
		else if (pkType.isUUID())
			generator.setGeneratorType(IDGeneratorTypeEnumeration.UUID);

		if (generator.getGeneratorType() == null) {
			addLog("The primary key for table '" + tableName + "' is mapped to an invalid Java type!", Status.ERROR);
			return null;
		}

		// If the target database supports sequences the generator type can be changed accordingly!
		if (generator.getGeneratorType() == IDGeneratorTypeEnumeration.IDENTITY && project.getDatabase().isSupportsSequence()) {
			if (sequences != null && !sequences.isEmpty()) {
				generator.setGeneratorType(IDGeneratorTypeEnumeration.SEQUENCE);
				generator.setBlockSize(SEQ_BLOCK_SIZE);
				generator.setInitialValue(SEQ_START_INDEX);

				boolean sequenceFound = false;

				// Search for a sequence that follows internal naming conventions
				for (final String sequenceName : sequences)
					if (sequenceName.equals(generator.getName())) {
						generator.setName(sequenceName);
						sequenceFound = true;
						break;
					}

				// Search for a sequence by using the domain object name
				if (!sequenceFound)
					for (final String sequenceName : sequences)
						if (sequenceName.toLowerCase().contains(domainObject.getName().toLowerCase())) {
							generator.setName(sequenceName);
							sequenceFound = true;
							break;
						}

				// If no sequence has been found we'll use the first sequence from the provided list
				if (!sequenceFound)
					sequences.stream().findFirst().ifPresent(generator::setName);
			}
			else
				generator.setGeneratorType(IDGeneratorTypeEnumeration.NONE);
		}

		return generator;
	}

	/**
	 * @param column
	 * @return the Java type due to a column type or null if no mapping could be found
	 */
	private JavaType getJavaTypeForColumn(DBColumn column) {
		final DBColumnType colType = column.getColumnType();
		final var fullColumnName = column.getDatabaseTable().getName() + "." + column.getName();

		if (!colType.getJavaTypes().isEmpty())
			return colType.getJavaTypes().get(0);

		addLog("A valid Java type for column '" + fullColumnName + "' could not be found!", Status.ERROR);

		return null;
	}

	/**
	 * @param table
	 * @return true if it is likely that the given table represents an association table for a many-to-many association
	 */
	private boolean isManyToManyAssoc(DBTable table) {
		boolean createManyToMany = false;

		// Check if it can really be used as many-to-many association
		if (table.getPrimaryKey() == null) {
			if (table.getColumns().size() == 2 && table.getForeignKeys().size() == 2)
				createManyToMany = true;
		}
		else if (table.getColumns().size() == 3 && table.getForeignKeys().size() == 2)
			createManyToMany = true;

		if (createManyToMany)
			manyToManyTables.add(table);

		return createManyToMany;
	}

	/**
	 * Create a domain object based on the meta-data of a database table
	 * @param table
	 */
	private void createDomainObject(DBTable table) {
		final DomainObject domainObject = DomainFactory.eINSTANCE.createDomainObject();
		final String name = convertToClassName(table.getName(), false);
		final String pluralName = convertToClassName(table.getName(), true);
		final var revEngObj = new RevEngDomainObject(revEngModel, defaultNamespace.getName(), domainObject, true);

		// Test if a domain object with the same name already exists
		for (final DomainObject existingObject : project.getAllDomainObjectsOfProject(true, true))
			if (existingObject.getName().equals(name)) {
				addLog("A domain object with the name '" + name + "' already exists locally!", Status.WARNING);
				return;
			}

		domainObject.setName(name);
		domainObject.setNamePlural(pluralName);
		domainObject.setDatabaseTable(table);
		domainObject.setLabel(EclipseIDEService.buildDefaultLabel(domainObject.getName()));
		domainObject.setLabelPlural(EclipseIDEService.buildDefaultPluralLabel(domainObject.getName()));
		domainObject.setComment("Domain object for " + domainObject.getLabel() + " objects");
		domainObject.setPropertyAccess(false);
		domainObject.setTag(DomainTagEnumeration.NONE);
		domainObject.setMappable(true);
		domainObject.setDiscriminatorValue("");
		domainObject.setIDGenerator(initIDGenerator(domainObject, table.getPrimaryKey()));

		// If no ID generator is available we cannot proceed!
		if (domainObject.getIDGenerator() == null)
			return;

		// Add the object to the domain model
		revEngModel.getDomainObjects().add(revEngObj);

		// Add domain attributes and associations
		initFields(table, revEngObj);

		addLog("Domain object '" + name + "' initialized!", Status.INFO);
	}

	/**
	 * Search for new attributes and associations of an existing domain object
	 * @param table
	 */
	private void editDomainObject(DBTable table) {
		DomainObject domainObject = null;
		RevEngDomainObject revEngObj = null;

		// Search for an existing domain object that is mapped to the given table
		for (final RevEngDomainObject obj : revEngModel.getDomainObjects()) {
			if (obj.getDomainObject().getDatabaseTable() == null)
				continue;

			if (table.getConvertedName().equals(obj.getDomainObject().getDatabaseTable().getConvertedName())) {
				revEngObj = obj;
				domainObject = obj.getDomainObject();
				break;
			}
		}

		if (domainObject == null) {
			addLog("A domain object for table '" + table.getName() + "' could not be found!", Status.ERROR);
			return;
		}

		// Add domain attributes and associations
		initFields(table, revEngObj);
	}

	/**
	 * @param table
	 * @param revEngObj
	 */
	private void initFields(DBTable table, RevEngDomainObject revEngObj) {
		final String pkColName = table.getPrimaryKey().getColumn().getConvertedName();
		DBTable existingTable = null;

		if (!revEngObj.isCreatedByReverseEngineering())
			existingTable = revEngObj.getDomainObject().getDatabaseTable();

		// Iterate over all columns
		for (final DBColumn newColumn : table.getColumns()) {
			boolean columnFound = false;

			// If the domain object already exists we must not add a field that is based on an existing column!
			if (existingTable != null)
				for (final DBColumn existingColumn : existingTable.getColumns())
					if (existingColumn.getConvertedName().equals(newColumn.getConvertedName())) {
						columnFound = true;
						break;
					}

			if (columnFound)
				continue;

			// The column is not part of the existing table!
			boolean addAttribute = true;

			// An association must not be created if the column represents the primary key column!
			if (!pkColName.equals(newColumn.getName())) {
				// If the column belongs to a foreign key we assume that an association must be created!
				for (final ForeignKey foreignKey : table.getForeignKeys())
					if (foreignKey.getColumn().getConvertedName().equals(newColumn.getConvertedName())) {
						addAttribute = false;
						break;
					}
			}

			if (addAttribute) {
				final DomainAttribute newAttribute = initDomainAttribute(revEngObj, newColumn);

				if (newAttribute == null)
					return;

				if (!revEngObj.isCreatedByReverseEngineering())
					addLog("New attribute '" + newAttribute.getName() + "' initialized!", Status.INFO);

				revEngObj.addAttribute(newAttribute, true);
			}
			else {
				final AbstractDomainAssociation assoc = initManyToOneAssociation(newColumn);

				if (!revEngObj.isCreatedByReverseEngineering())
					addLog("New many-to-one association '" + assoc.getName() + "' initialized!", Status.INFO);

				revEngObj.addAssociation(assoc, true);
			}
		}
	}

	/**
	 * Convert the table name to a valid Java class name
	 * @param tableName
	 * @param pluralForm
	 * @return the converted name
	 */
	private String convertToClassName(String tableName, boolean pluralForm) {
		String className = tableName;

		if (className.toLowerCase().endsWith(TABLE_SUFFIX_SHORT) && className.length() > TABLE_SUFFIX_SHORT.length())
			className = className.substring(0, className.length() - TABLE_SUFFIX_SHORT.length());

		if (className.toLowerCase().endsWith(TABLE_SUFFIX_LONG) && className.length() > TABLE_SUFFIX_LONG.length())
			className = className.substring(0, className.length() - TABLE_SUFFIX_LONG.length());

		return convertToJavaName(className, true, pluralForm);
	}

	/**
	 * Convert the original name in the database to a valid Java name. Attention: We assume that the name is provided in singular
	 * form!
	 * @param name
	 * @param startUpperCase
	 * @param pluralForm
	 * @return the converted name
	 */
	private String convertToJavaName(String name, boolean startUpperCase, boolean pluralForm) {
		final var b = new StringBuilder();
		String convName;
		boolean nextUpperCase = false;

		if (name.isEmpty())
			return b.toString();

		// Iterate over all characters. In case of a special character the next character is changed to upper-case and the original
		// character is removed!
		for (final char c : name.toCharArray())
			if (c == '_' || c == '-' || c == ' ' || c == '"' || c == '.')
				nextUpperCase = true;
			else if (nextUpperCase) {
				nextUpperCase = false;
				b.append(Character.toUpperCase(c));
			}
			else
				b.append(Character.toLowerCase(c));

		convName = b.toString();

		if (pluralForm)
			convName = EclipseIDEService.buildDefaultPluralForm(convName);

		if (startUpperCase)
			return convName.substring(0, 1).toUpperCase() + convName.substring(1);

		return convName;
	}

	/**
	 * @param revEngObject
	 * @param column
	 * @return the created domain attribute or null if it could not be initialized
	 */
	private DomainAttribute initDomainAttribute(RevEngDomainObject revEngObject, DBColumn column) {
		final String tableName = column.getDatabaseTable().getName();
		final String columnName = column.getName();

		final DomainAttribute attr = DomainFactory.eINSTANCE.createDomainAttribute();
		attr.setColumn(column);
		attr.setInsertable(true);
		attr.setUpdatable(true);
		attr.setName(convertToJavaName(column.getName(), false, false));
		attr.setLabel(EclipseIDEService.buildDefaultLabel(attr.getName()));
		attr.setLabelPlural(EclipseIDEService.buildDefaultPluralLabel(attr.getName()));
		attr.setPersistent(true);
		attr.setTag(AttributeTagEnumeration.NONE);
		attr.setJavaType(getJavaTypeForColumn(column));
		attr.setFetchTypeEager(true);

		if (attr.getJavaType() == null) {
			addLog("A valid Java type for column '" + columnName + "' of table '" + tableName + "' could not be found!", Status.ERROR);
			return null;
		}

		// Check if this attribute is mapped to the primary key column
		if (column.getConvertedName().equals(column.getDatabaseTable().getPrimaryKey().getColumn().getConvertedName())) {
			attr.setPk(true);

			if (attr.getJavaType().isByteArray()) {
				final List<JavaType> javaTypes = attr.getColumn().getColumnType().getJavaTypes();
				final boolean canBeMappedToUUID = javaTypes.stream().map(JavaType::getName).anyMatch(name -> name.equals(JavaType.UUID));

				if (canBeMappedToUUID)
					attr.setJavaType(project.getJavaTypeByName(JavaType.UUID));
			}
		}

		if (attr.isPk() && !(attr.getJavaType().isIntegerOrLong() || attr.getJavaType().isString() || attr.getJavaType().isUUID())) {
			addLog("The type '" + attr.getJavaType().getName() + "' is not supported for primary key attributes!", Status.ERROR);
			return null;
		}

		if (attr.getJavaType().isByteArray()) {
			attr.setFetchTypeEager(false);
			attr.setLob(true);
		}

		if (attr.getJavaType().isDateOrCalendar())
			attr.setTemporalType(TemporalTypeEnumeration.TIMESTAMP);

		// Check if this attribute can be used as a display attribute
		setDisplayAttribute(revEngObject, attr);

		// Check if this attribute can be used as a version attribute
		setVersionAttribute(revEngObject, attr);

		// Check if this attribute should be set when performing a persist operation
		setDateOnPersist(attr);

		// Check if this attribute should be set when performing an update operation
		setDateOnUpdate(attr);

		final DomainAttributeValidator validator = DomainFactory.eINSTANCE.createDomainAttributeValidator();
		validator.setNullable(column.isNullable());
		validator.setRegularExpression("");
		validator.setMinValue("");
		validator.setMaxValue("");

		// Exchange the attribute's type if it is mapped to a primitive type and the column can be null
		if (column.isNullable() && attr.getJavaType().isPrimitive())
			attr.setJavaType(project.getJavaTypeByName(attr.getJavaType().getWrapperTypeName()));

		// If a String attribute is mapped to a column that has a not-null constraint we assume that an empty String is not allowed!
		if (!column.isNullable() && attr.getJavaType().isString())
			validator.setMinLength(1);

		if (column.getLength() > 0)
			validator.setMaxLength(column.getLength());

		attr.setDomainAttributeValidator(validator);

		return attr;
	}

	/**
	 * Determine if the attribute represents a display attribute
	 * @param revEngObject
	 * @param attr
	 */
	private void setDisplayAttribute(RevEngDomainObject revEngObject, DomainAttribute attr) {
		final DBTable sourceTable = getSourceTable(revEngObject.getDomainObject().getDatabaseTable());

		// A display attribute must be of type String!
		if (sourceTable == null || !attr.getJavaType().isString())
			return;

		// Check if the domain object already contains a display attribute!
		for (final RevEngDomainAttribute revEngAttr : revEngObject.getAttributes())
			if (revEngAttr.getDomainAttribute().isDisplayAttribute())
				return;

		for (final DBIndex index : sourceTable.getIndexes()) {
			// The index must both represent a unique key and must have only one column!
			if (!index.isUnique() || index.getColumns().size() != 1)
				continue;

			// Check if the attribute's column represents the column of the unique key
			for (final DBColumn col : index.getColumns())
				if (col.getConvertedName().equals(attr.getColumn().getConvertedName())) {
					attr.setDisplayAttribute(true);
					return;
				}
		}
	}

	/**
	 * Determine if the attribute represents a version attribute
	 * @param revEngObject
	 * @param attr
	 */
	private void setVersionAttribute(RevEngDomainObject revEngObject, DomainAttribute attr) {
		// A version attribute must be of type int, Integer, long or Long!
		if (!attr.getJavaType().isIntegerOrLong())
			return;

		// Check if the domain object already contains a version attribute!
		for (final RevEngDomainAttribute revEngAttr : revEngObject.getAttributes())
			if (revEngAttr.getDomainAttribute().isTrackVersion())
				return;

		if (attr.getName().equals(configuration.getVersionAttrName()))
			attr.setTrackVersion(true);
	}

	/**
	 * Determine if the attribute's value should be set while performing a persist operation
	 * @param attr
	 */
	private void setDateOnPersist(DomainAttribute attr) {
		// This is only allowed for attributes that represent a date
		if (!attr.getJavaType().isTemporalType())
			return;

		if (attr.getName().equals(configuration.getDateOnPersistAttrName()))
			attr.setSetDateOnPersist(true);
	}

	/**
	 * Determine if the attribute's value should be set when performing an update operation
	 * @param attr
	 */
	private void setDateOnUpdate(DomainAttribute attr) {
		// This is only allowed for attributes that represent a date
		if (!attr.getJavaType().isTemporalType())
			return;

		if (attr.getName().equals(configuration.getDateOnUpdateAttrName()) && attr.getColumn().isNullable())
			attr.setSetDateOnUpdate(true);
	}

	/**
	 * Initialize a many-to-one association
	 * @param column
	 * @return the new association
	 */
	private ManyToOneAssociation initManyToOneAssociation(DBColumn column) {
		final ManyToOneAssociation assoc = DomainFactory.eINSTANCE.createManyToOneAssociation();
		assoc.setColumn(column);
		assoc.setInsertable(true);
		assoc.setUpdatable(true);
		assoc.setOptional(column.isNullable());
		assoc.setOwner(true);
		assoc.setTag(AssociationTagEnumeration.NONE);
		assoc.setName(convertToJavaName(column.getName(), false, false));

		return assoc;
	}

	/**
	 * Initialize all many-to-many associations
	 */
	private void initManyToManyAssociations() {
		manyToManyTables.forEach(this::initManyToManyAssociation);
	}

	/**
	 * Initialize a many-to-many association by using a given association table
	 * @param assocTable
	 */
	private void initManyToManyAssociation(DBTable assocTable) {
		DBColumn col1 = null;
		DBColumn col2 = null;

		for (final DBColumn col : assocTable.getColumns()) {
			if (assocTable.getPrimaryKey() != null && col.equals(assocTable.getPrimaryKey().getColumn()))
				continue;

			if (col1 == null)
				col1 = col;
			else
				col2 = col;
		}

		if (col1 == null || col2 == null) {
			addLog("At least one required column of many-to-many table '" + assocTable.getName() + "' could not be found!",
					Status.ERROR);
			return;
		}

		// Check if the association table is already used locally!
		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects()) {
			final DBTable table = revEngObj.getDomainObject().getDatabaseTable();

			if (table != null && table.getConvertedName().equals(assocTable.getConvertedName()))
				return;

			for (final RevEngDomainAssociation revEngAssoc : revEngObj.getAssociations()) {
				if (!revEngAssoc.isManyToMany())
					continue;

				final var mtm = (ManyToManyAssociation) revEngAssoc.getAssociation();

				if (mtm.getTable() != null && mtm.getTable().getConvertedName().equals(assocTable.getConvertedName()))
					return;
			}
		}

		final DomainObject domainObject1 = getAssociationTarget(assocTable, col1);
		final DomainObject domainObject2 = getAssociationTarget(assocTable, col2);

		// If at least one of both domain objects is not available we cannot proceed!
		if (domainObject1 == null || domainObject2 == null)
			return;

		if (!initColumnTypes(assocTable))
			return;

		final RevEngDomainObject revEngObj1 = revEngModel.searchRevEngObjectByDomainObject(domainObject1);
		final RevEngDomainObject revEngObj2 = revEngModel.searchRevEngObjectByDomainObject(domainObject2);

		if (revEngObj1 == null || revEngObj2 == null)
			return;

		final String ownerName = domainObject2.getNamePlural().substring(0, 1).toLowerCase()
				+ domainObject2.getNamePlural().substring(1);
		String reverseName = domainObject1.getNamePlural().substring(0, 1).toLowerCase() + domainObject1.getNamePlural().substring(1);

		if (ownerName.equals(reverseName))
			reverseName = REVERSE_NAME_SUFFIX + domainObject2.getNamePlural();

		final ManyToManyAssociation mtmOwner = DomainFactory.eINSTANCE.createManyToManyAssociation();
		mtmOwner.setName(ownerName);
		mtmOwner.setTable(assocTable);
		mtmOwner.setOwner(true);
		mtmOwner.setTag(AssociationTagEnumeration.NONE);
		mtmOwner.setTarget(domainObject2);

		final ManyToManyAssociation mtmReverse = DomainFactory.eINSTANCE.createManyToManyAssociation();
		mtmReverse.setName(reverseName);
		mtmReverse.setReverseAssociation(mtmOwner);
		mtmReverse.setOwner(false);
		mtmReverse.setTag(AssociationTagEnumeration.NONE);
		mtmReverse.setTarget(domainObject1);

		mtmOwner.setReverseAssociation(mtmReverse);

		revEngObj1.addAssociation(mtmOwner, true);
		revEngObj2.addAssociation(mtmReverse, true);

		if (!revEngObj1.isCreatedByReverseEngineering()) {
			addLog("New many-to-many association '" + mtmOwner.getName() + "' initialized!", Status.INFO);
			addLog("New many-to-many association '" + mtmReverse.getName() + "' initialized!", Status.INFO);
		}
	}

	/**
	 * Initialize all many-to-one association targets
	 */
	private void initManyToOneAssociationTargets() {
		for (final RevEngDomainObject obj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = obj.getDomainObject();

			if (domainObject.getDatabaseTable() == null)
				continue;

			for (final RevEngDomainAssociation assocObj : obj.getAssociations())
				if (assocObj.isCreatedByReverseEngineering() && assocObj.isManyToOne()) {
					final var mto = (ManyToOneAssociation) assocObj.getAssociation();

					// In any case, we must use the table generated by the reverse engineering process in order to find the association
					// target!
					final DBTable sourceTable = getSourceTable(domainObject.getDatabaseTable());

					if (sourceTable == null)
						continue;

					final DomainObject targetDomainObject = getAssociationTarget(sourceTable, mto.getColumn());

					mto.setTarget(targetDomainObject);
				}
		}
	}

	/**
	 * @param table
	 * @param column
	 * @return the target domain object of a domain association
	 */
	private DomainObject getAssociationTarget(DBTable table, DBColumn column) {
		final var fullColumnName = table.getName() + "." + column.getName();
		final DBTable targetTable = table.getForeignKeys().stream()
				.filter(fk -> fk.getReferencedColumn() != null && fk.getColumn().getConvertedName().equals(column.getConvertedName()))
				.findFirst().map(fk -> fk.getReferencedColumn().getDatabaseTable()).orElse(null);

		if (targetTable == null) {
			addLog("The association target table based on column '" + fullColumnName + "' could not be found!", Status.ERROR);
			return null;
		}

		for (final RevEngDomainObject obj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = obj.getDomainObject();

			if (domainObject.getDatabaseTable() == null)
				continue;

			if (domainObject.getDatabaseTable().getConvertedName().equals(targetTable.getConvertedName()))
				return domainObject;
		}

		addLog("The association target domain object based on column '" + fullColumnName + "' could not be found!", Status.ERROR);

		return null;
	}

	/**
	 * @param table
	 * @return the corresponding source table
	 */
	private DBTable getSourceTable(DBTable table) {
		return sourceDBModel.getDatabaseTables().stream()
				.filter(sourceTable -> sourceTable.getConvertedName().equals(table.getConvertedName())).findFirst().orElse(null);
	}

	/**
	 * @param msg
	 * @param status
	 */
	private void addLog(String msg, Status status) {
		logEntries.add(new ReverseEngineeringLogEntry(msg, status, ReverseEngineeringLogEntry.Source.IMPORT));
	}

}
