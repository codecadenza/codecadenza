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
package net.codecadenza.eclipse.generator.domain;

import static net.codecadenza.eclipse.shared.Constants.LISTENER_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_LISTENER;

import java.util.HashMap;
import java.util.Optional;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAssociationComparator;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeComparator;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Generator for domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectGenerator extends AbstractJavaSourceGenerator {
	private static final String COLLECTION = JavaTypeModifierEnumeration.COLLECTION.toString();

	private final DomainObject domainObject;
	private final Project project;
	private final HashMap<String, String> namedQueryMap = new HashMap<>();
	private boolean addListener;
	private boolean addStandardValidationImport;
	private boolean useInternalValidation = true;
	private String generatorName;

	/**
	 * Constructor
	 * @param domainObject
	 */
	public DomainObjectGenerator(DomainObject domainObject) {
		super(domainObject.getSourceFile());

		this.domainObject = domainObject;
		this.project = domainObject.getNamespace().getProject();

		// Determine the field validation strategy
		if (project.getValidationType() == ValidationTypeEnumeration.STANDARD)
			this.useInternalValidation = false;

		// Test if the entity class requires a listener
		for (final DomainAttribute attr : domainObject.getAttributes())
			if (attr.isSetDateOnPersist() || attr.isSetDateOnUpdate()) {
				this.addListener = true;
				break;
			}

		if (domainObject.getParent() == null) {
			if (domainObject.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.TABLE)
				this.generatorName = domainObject.getName().toUpperCase() + "_ID_GEN";
			else
				this.generatorName = domainObject.getIDGenerator().getName();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (useInternalValidation)
			importPackage(PACK_VALIDATION);

		if (addStandardValidationImport)
			importPackage("jakarta.validation.constraints");

		importPackage("jakarta.persistence");

		for (final DomainAttribute attr : domainObject.getAttributes())
			if (attr.getJavaType().getNamespace() != null
					&& !attr.getJavaType().getNamespace().toString().equals(domainObject.getNamespace().toString()))
				importPackage(attr.getJavaType().getNamespace().toString());

		if (domainObject.getPKAttribute().getJavaType().getNamespace() != null)
			importPackage(domainObject.getPKAttribute().getJavaType().getNamespace().toString());

		domainObject.getAssociations().forEach(assoc -> {
			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
				importPackage("java.util");

			if (!domainObject.getNamespace().equals(assoc.getTarget().getNamespace()))
				importPackage(assoc.getTarget().getNamespace().toString());
		});

		if (domainObject.getParent() != null
				&& !domainObject.getNamespace().getName().equals(domainObject.getParent().getNamespace().getName()))
			importPackage(domainObject.getParent().getNamespace().toString());

		if (addListener)
			importClass(domainObject.getNamespace().toString() + SUB_PACKAGE_LISTENER + "." + domainObject.getName() + LISTENER_SUFFIX);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		boolean addTableAnnotation = false;

		if (domainObject.isMappedSuperClass())
			b.append("@MappedSuperclass\n");
		else {
			final DomainObject parentDomainObject = domainObject.getParent();

			b.append("@Entity\n");

			if (parentDomainObject == null)
				addTableAnnotation = true;
			else {
				if (parentDomainObject.isMappedSuperClass())
					addTableAnnotation = true;

				if (parentDomainObject.getInheritanceType() == InheritanceTypeEnumeration.JOINED)
					addTableAnnotation = true;
			}

			if (addTableAnnotation) {
				final DBTable table = domainObject.getDatabaseTable();

				b.append("@Table(name=\"" + table.getMappingName() + "\"");
				b.append(addSchemaAndCatalogAnnotationParams(table));
				b.append(")\n");
			}

			if (domainObject.getInheritanceType() != InheritanceTypeEnumeration.NONE
					&& domainObject.getRootParentDomainObject(false) == domainObject) {
				b.append("@Inheritance(strategy=InheritanceType." + domainObject.getInheritanceType().toString() + ")\n");
				b.append("@DiscriminatorColumn(name=\"" + domainObject.getDiscriminatorColumn().getMappingName());
				b.append("\", discriminatorType=DiscriminatorType." + domainObject.getDiscriminatorColumnType().toString() + ")\n");
				b.append("@DiscriminatorValue(\"" + domainObject.getDiscriminatorValue() + "\")\n");
			}
			else if (parentDomainObject != null && domainObject.getInheritanceType() != InheritanceTypeEnumeration.NONE) {
				b.append("@DiscriminatorValue(\"" + domainObject.getDiscriminatorValue() + "\")\n");

				if (domainObject.getInheritanceType() == InheritanceTypeEnumeration.JOINED) {
					final String pkColDomainObject = domainObject.getDatabaseTable().getPrimaryKey().getColumn().getMappingName();
					final String pkColParentObject = parentDomainObject.getDatabaseTable().getPrimaryKey().getColumn().getMappingName();

					if (!pkColDomainObject.equals(pkColParentObject)) {
						b.append("@PrimaryKeyJoinColumn(name = \"" + pkColDomainObject);
						b.append("\", referencedColumnName = \"" + pkColParentObject + "\")");
					}
				}
			}
		}

		if (domainObject.getParent() == null) {
			if (domainObject.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.SEQUENCE) {
				final IDGenerator generator = domainObject.getIDGenerator();

				b.append("@SequenceGenerator(name=\"" + generatorName + "\", ");
				b.append(addSchemaAndCatalogAnnotationParams(domainObject.getDatabaseTable()));
				b.append("sequenceName=\"" + DBNamingUtil.convertToMapping(generatorName, project.getDatabase()) + "\", ");
				b.append("allocationSize=" + generator.getBlockSize() + ")\n");
			}
			else if (domainObject.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.TABLE) {
				final Database database = project.getDatabase();
				final String idTableName = DBNamingUtil.convertToMapping("GEN_ID_TAB", database);
				final String pkColName = DBNamingUtil.convertToMapping("ID_NAME", database);
				final String valueColName = DBNamingUtil.convertToMapping("ID_VALUE", database);
				final String schemaName = database.getSchemaName() == null ? ""
						: DBNamingUtil.convertToMapping(database.getSchemaName(), database);
				final String catalogName = database.getCatalogName() == null ? ""
						: DBNamingUtil.convertToMapping(database.getCatalogName(), database);

				b.append("@TableGenerator(name=\"" + generatorName + "\", table=\"");
				b.append(idTableName + "\", pkColumnName=\"" + pkColName + "\", ");
				b.append("valueColumnName=\"" + valueColName + "\", pkColumnValue=\"" + domainObject.getIDGenerator().getName() + "\" ");

				if (!schemaName.isEmpty())
					b.append(", schema=\"" + schemaName + "\"");

				if (!catalogName.isEmpty())
					b.append(", catalog=\"" + catalogName + "\"");

				b.append(", allocationSize=" + domainObject.getIDGenerator().getBlockSize() + ")\n");
			}
		}

		if (addListener)
			b.append("@EntityListeners(" + domainObject.getName() + LISTENER_SUFFIX + ".class)\n");

		// Add named queries
		if (!domainObject.isMappedSuperClass())
			b.append(addNamedQueries());

		b.append("public ");

		// Even if a domain object is marked as abstract the generated source file should not contain the keyword "abstract" in order
		// to avoid problems in other classes!
		b.append("class ");

		if (domainObject.getParent() != null)
			b.append(domainObject.getName() + " extends " + domainObject.getParent().getName());
		else
			b.append(domainObject.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (!domainObject.isMappedSuperClass())
			namedQueryMap.entrySet().forEach(entry -> addPublicConstant(JavaType.STRING, entry.getKey(),
					"\"" + domainObject.getName() + "." + entry.getValue() + "\"").create());

		// Create fields for all domain attributes
		domainObject.getAttributes().forEach(attr -> {
			final JavaFieldGenerator fieldGenerator = addPrivateField(attr.getJavaType().getName(), attr.getName());

			if (!domainObject.isPropertyAccess()) {
				fieldGenerator.withAnnotations(addPersistenceAnnotations(attr));

				if (!useInternalValidation) {
					final String validationAnnotation = addValidationAnnotations(attr);

					if (!validationAnnotation.isEmpty()) {
						addStandardValidationImport = true;
						fieldGenerator.withAnnotations(validationAnnotation);
					}
				}
			}

			fieldGenerator.create();
		});

		// Create fields for all domain association
		domainObject.getAssociations().forEach(assoc -> {
			String typeName = assoc.getTarget().getName();

			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
				typeName = COLLECTION + "<" + assoc.getTarget().getName() + ">";

			final JavaFieldGenerator fieldGenerator = addPrivateField(typeName, assoc.getName());

			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
				fieldGenerator.withDefaultValue("new ArrayList<>()");

			if (!domainObject.isPropertyAccess()) {
				if (assoc instanceof final ManyToManyAssociation mtm)
					fieldGenerator.withAnnotations(addManyToManyAnnotation(mtm));
				else if (assoc instanceof final OneToOneAssociation oto)
					fieldGenerator.withAnnotations(addOneToOneAnnotation(oto));
				else if (assoc instanceof final ManyToOneAssociation mto)
					fieldGenerator.withAnnotations(addManyToOneAnnotation(mto));
				else if (assoc instanceof final OneToManyAssociation otm)
					fieldGenerator.withAnnotations(addOneToManyAnnotation(otm));

				if (!useInternalValidation) {
					final String validationAnnotation = addValidationAnnotation(assoc);

					if (!validationAnnotation.isEmpty()) {
						addStandardValidationImport = true;

						fieldGenerator.withAnnotations(validationAnnotation);
					}
				}
			}

			fieldGenerator.create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var b = new StringBuilder();
		b.append("/**\n * Default constructor\n */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + domainObject.getName() + "()\n{\n}\n\n");

		addConstructor(domainObject.getName() + "()", b.toString());

		if (domainObject.getParent() == null) {
			var signature = domainObject.getName() + "(" + domainObject.getPKAttribute().getJavaType().getName() + " ";
			signature += domainObject.getPKAttribute().getName() + ")";

			b = new StringBuilder();
			b.append("/**\n * Constructor using primary key field\n");
			b.append(" * @param " + domainObject.getPKAttribute().getName() + "\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + signature + "\n");
			b.append("{\n");
			b.append("this." + domainObject.getPKAttribute().getName() + " = " + domainObject.getPKAttribute().getName() + ";\n");
			b.append("}\n\n");

			addConstructor(signature, b.toString());

			final DomainAttribute displayAttribute = domainObject.getDisplayAttribute();

			if (displayAttribute != null) {
				signature = domainObject.getName() + "(" + domainObject.getPKAttribute().getJavaType().getName() + " ";
				signature += domainObject.getPKAttribute().getName() + ", ";
				signature += displayAttribute.getJavaType().getName() + " " + displayAttribute.getName() + ")";

				b = new StringBuilder();
				b.append("/**\n * Constructor using primary key field and display attribute\n");
				b.append(" * @param " + domainObject.getPKAttribute().getName() + "\n");
				b.append(" * @param " + displayAttribute.getName() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + signature + "\n");
				b.append("{\n");
				b.append("this." + domainObject.getPKAttribute().getName() + " = " + domainObject.getPKAttribute().getName() + ";\n");
				b.append("this." + displayAttribute.getName() + " = " + displayAttribute.getName() + ";\n");
				b.append("}\n\n");

				addConstructor(signature, b.toString());
			}
		}
		else {
			var signature = domainObject.getName() + "(" + domainObject.getPKAttribute().getJavaType().getName() + " ";
			signature += domainObject.getPKAttribute().getName() + ")";

			b = new StringBuilder();
			b.append("/**\n * Constructor using primary key field\n");
			b.append(" * @param " + domainObject.getPKAttribute().getName() + "\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + signature + "\n");
			b.append("{\n");
			b.append("super(" + domainObject.getPKAttribute().getName() + ");\n");
			b.append("}\n\n");

			addConstructor(signature, b.toString());

			final DomainAttribute displayAttribute = domainObject.getDisplayAttribute();

			if (displayAttribute != null) {
				signature = domainObject.getName() + "(" + domainObject.getPKAttribute().getJavaType().getName() + " ";
				signature += domainObject.getPKAttribute().getName() + ", ";
				signature += displayAttribute.getJavaType().getName() + " " + displayAttribute.getName() + ")";

				b = new StringBuilder();
				b.append("/**\n * Constructor using primary key field and display attribute\n");
				b.append(" * @param " + domainObject.getPKAttribute().getName() + "\n");
				b.append(" * @param " + displayAttribute.getName() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + signature + "\n");
				b.append("{\n");

				// We must check if the display attribute belongs to this domain object or to the parent!
				final boolean displayAttrBelongsToThis = domainObject.getAttributes().stream()
						.anyMatch(DomainAttribute::isDisplayAttribute);

				if (displayAttrBelongsToThis) {
					b.append("super(" + domainObject.getPKAttribute().getName() + ");\n\n");
					b.append("this." + displayAttribute.getName() + " = " + displayAttribute.getName() + ";\n");
				}
				else
					b.append("super(" + domainObject.getPKAttribute().getName() + ", " + displayAttribute.getName() + ");\n");

				b.append("}\n\n");

				addConstructor(signature, b.toString());
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		StringBuilder b;

		// Add getters and setters for all attributes
		for (final DomainAttribute attr : domainObject.getAttributes()) {
			final var setterIdentifier = "void " + attr.getSetterName() + "(" + attr.getJavaType().getName() + " " + attr.getName()
					+ ")";
			final var getterIdentifier = attr.getJavaType().getName() + " " + attr.getGetterName();
			b = new StringBuilder();

			if (attr.getJavaType().isBoolean())
				b.append("/**\n * @return true if the " + attr.getLabel() + " flag is set\n */\n");
			else
				b.append("/**\n * @return the " + attr.getLabel() + "\n */\n");

			if (domainObject.isPropertyAccess()) {
				b.append(addPersistenceAnnotations(attr));

				final String validationAnnotation = addValidationAnnotations(attr);

				b.append(validationAnnotation);

				if (!validationAnnotation.isEmpty() && !useInternalValidation)
					addStandardValidationImport = true;
			}
			else if (useInternalValidation) {
				// Internal validation annotations must be placed on getters no matter what kind of JPA annotation strategy is selected!
				b.append(addValidationAnnotations(attr));
			}

			b.append(getAnnotationForGeneratedElement());
			b.append("public " + getterIdentifier + "\n{\nreturn this." + attr.getName() + ";\n}\n\n");

			addMethod(getterIdentifier, b.toString());

			b = new StringBuilder();

			if (attr.getJavaType().isBoolean())
				b.append("/**\n * @param " + attr.getName() + " the value of the " + attr.getLabel() + " flag to set\n */\n");
			else
				b.append("/**\n * @param " + attr.getName() + " the " + attr.getLabel() + " to set\n */\n");

			b.append(getAnnotationForGeneratedElement());
			b.append("public " + setterIdentifier + "\n{\nthis." + attr.getName() + " = " + attr.getName() + ";\n}\n\n");

			addMethod(setterIdentifier, b.toString());
		}

		// Add getters and setters for all associations
		for (final AbstractDomainAssociation assoc : domainObject.getAssociations()) {
			String setterIdentifier;
			String getterIdentifier;
			b = new StringBuilder();

			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
				getterIdentifier = assoc.getTarget().getName() + " " + assoc.getGetterName();
				setterIdentifier = "void " + assoc.getSetterName() + "(" + assoc.getTarget().getName() + " " + assoc.getName() + ")";
			}
			else {
				getterIdentifier = COLLECTION + "<" + assoc.getTarget().getName() + "> " + assoc.getGetterName();
				setterIdentifier = "void " + assoc.getSetterName() + "(" + COLLECTION + "<" + assoc.getTarget().getName() + "> "
						+ assoc.getName() + ")";
			}

			// Add the comment
			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation)
				b.append("/**\n * @return the " + assoc.getTarget().getLabel() + "\n */\n");
			else
				b.append("/**\n * @return a collection of " + assoc.getTarget().getLabelPlural() + "\n */\n");

			if (domainObject.isPropertyAccess()) {
				if (assoc instanceof final ManyToManyAssociation mtm)
					b.append(addManyToManyAnnotation(mtm));
				else if (assoc instanceof final OneToOneAssociation oto)
					b.append(addOneToOneAnnotation(oto));
				else if (assoc instanceof final ManyToOneAssociation mto)
					b.append(addManyToOneAnnotation(mto));
				else if (assoc instanceof final OneToManyAssociation otm)
					b.append(addOneToManyAnnotation(otm));
			}

			addMethod(getterIdentifier, b.toString());

			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation) {
				b = new StringBuilder();

				if (domainObject.isPropertyAccess() || useInternalValidation) {
					final String validationAnnotation = addValidationAnnotation(assoc);

					b.append(validationAnnotation);

					if (!validationAnnotation.isEmpty() && !useInternalValidation)
						addStandardValidationImport = true;
				}

				b.append(getAnnotationForGeneratedElement());
				b.append("public " + getterIdentifier + "\n{\nreturn this." + assoc.getName() + ";\n}\n\n");

				addMethod(getterIdentifier, b.toString());

				b = new StringBuilder();
				b.append("/**\n * @param " + assoc.getName() + " the " + assoc.getTarget().getLabel() + " to set\n */\n");
			}
			else {
				b = new StringBuilder();
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + getterIdentifier + "\n{\nreturn this." + assoc.getName() + ";\n}\n\n");

				addMethod(getterIdentifier, b.toString());

				b = new StringBuilder();
				b.append("/**\n * @param " + assoc.getName() + " the " + assoc.getTarget().getLabelPlural() + " to set\n */\n");
			}

			b.append(getAnnotationForGeneratedElement());
			b.append("public " + setterIdentifier + "\n{\nthis." + assoc.getName() + " = " + assoc.getName() + ";\n}\n\n");

			addMethod(setterIdentifier, b.toString());
		}

		final String pkGetter = domainObject.getPKAttribute().getGetterName();
		final JavaType pkType = domainObject.getPKAttribute().getJavaType();

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Object#equals(java.lang.Object)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public boolean equals(Object obj)\n{\n");
		b.append("if(this == obj)\nreturn true;\n\n");
		b.append("if(obj == null)\nreturn false;\n\n");
		b.append("if(getClass() != obj.getClass())\nreturn false;\n\n");
		b.append("final var bean = (" + domainObject.getName() + ") obj;\n\n");

		if (!pkType.isPrimitive()) {
			b.append("if(" + domainObject.getPKAttribute().getGetterName() + " == null)\n");
			b.append("{\n");
			b.append("if(bean." + pkGetter + " != null)\n");
			b.append("return false;\n");
			b.append("}\n");
			b.append("else if(!" + domainObject.getPKAttribute().getGetterName() + ".equals(bean." + pkGetter + "))\n");
			b.append("return false;\n\n");
			b.append("return true;\n");
		}
		else
			b.append("return " + domainObject.getPKAttribute().getGetterName() + " == bean." + pkGetter + ";\n");

		b.append("}\n\n");

		addMethod("boolean equals(Object obj)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Object#hashCode()\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public int hashCode()\n{\n");

		if (!domainObject.getPKAttribute().getJavaType().isPrimitive()) {
			b.append("// Return hash code of current date if primary key field is not yet set!\n");
			b.append("if(" + domainObject.getPKAttribute().getGetterName() + " == null)\n");
			b.append("return new java.util.Date().hashCode();\n\n");
			b.append("return " + domainObject.getPKAttribute().getGetterName() + ".hashCode();\n");
		}
		else if (pkType.isType(JavaType.LONG)) {
			b.append("return (int) (" + domainObject.getPKAttribute().getGetterName());
			b.append(" ^ (" + domainObject.getPKAttribute().getGetterName() + " >>> 32));\n");
		}
		else
			b.append("return " + domainObject.getPKAttribute().getGetterName() + ";\n");

		b.append("}\n\n");

		addMethod("int hashCode()", b.toString());

		if (useInternalValidation) {
			b = new StringBuilder();

			if (domainObject.getParent() != null) {
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see " + domainObject.getParent().getNamespace().toString() + ".");
				b.append(domainObject.getParent().getName() + "#validate()\n");
				b.append(" */\n");
			}
			else
				b.append("/**\n * Validate the object before performing an update or create operation\n */\n");

			b.append("@PreUpdate\n");
			b.append("@PrePersist\n");
			b.append(getAnnotationForGeneratedElement());

			if (domainObject.getParent() != null)
				b.append("@Override\n");

			b.append("protected void validate()\n{\nfinal var validator = new BeanValidator(this);\nvalidator.validate();\n}\n\n");

			addMethod("void validate()", b.toString());
		}
	}

	/**
	 * @return true if a listener should be added
	 */
	public boolean isAddListener() {
		return addListener;
	}

	/**
	 * @param uniqueAttribute
	 * @param domainObjectName
	 * @param pkAttribute
	 * @return the generated named queries that are based on a given domain object attribute
	 */
	private String createNamedQueriesOfAttr(DomainAttribute uniqueAttribute, String domainObjectName, DomainAttribute pkAttribute) {
		final var b = new StringBuilder();
		final String ukAttrName = uniqueAttribute.getUpperCaseName();
		final var getByName = "getBy" + ukAttrName;
		final var findByName = "findBy" + ukAttrName;
		final var checkName = "checkBy" + ukAttrName;
		final var queryName = checkName + "And" + pkAttribute.getUpperCaseName();
		final var paramName = uniqueAttribute.getName();
		final var paramID = pkAttribute.getName();

		// Avoid adding named queries twice!
		if (namedQueryMap.containsKey("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase()))
			return b.toString();

		namedQueryMap.put("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase(), getByName);
		namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase(), checkName);
		namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_" + pkAttribute.getName().toUpperCase(), queryName);

		// Add a named query to find the domain object by the unique key
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_FIND_BY_" + ukAttrName.toUpperCase() + ", query=\"select a from ");
		b.append(domainObjectName + " a where a." + uniqueAttribute.getName() + " = :" + paramName + "\")\n");

		// Add a named query to search domain objects by the unique key
		if (uniqueAttribute.getJavaType().isString()) {
			b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_SEARCH_BY_");
			b.append(ukAttrName.toUpperCase() + ", query=\"select a from ");
			b.append(domainObjectName + " a where a." + uniqueAttribute.getName() + " like :" + paramName + "\")\n");

			namedQueryMap.put("NQ_UK_SEARCH_BY_" + ukAttrName.toUpperCase(), findByName);
		}

		// Add a named query to check the domain object by the unique key
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_");
		b.append(ukAttrName.toUpperCase() + ", query=\"select count(a) from ");
		b.append(domainObjectName + " a where a." + uniqueAttribute.getName() + " = :" + paramName + "\")\n");

		// Add a named query to check the domain object by the unique key and the primary key
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
		b.append("_AND_" + pkAttribute.getName().toUpperCase() + ", query=\"select count(a) from ");
		b.append(domainObjectName + " a where a." + uniqueAttribute.getName() + " = :" + paramName);
		b.append(" and a." + pkAttribute.getName() + " <> :" + paramID + "\")\n");

		return b.toString();
	}

	/**
	 * Add named queries
	 * @return the named queries
	 */
	private String addNamedQueries() {
		final var b = new StringBuilder();
		final String domainObjectName = domainObject.getName();
		final DomainAttribute pkAttribute = domainObject.getPKAttribute();
		final DomainAttribute displayAttribute = domainObject.getDisplayAttribute();

		// It could be the case that a domain object has a display attribute that in turn has no unique key!
		if (displayAttribute != null)
			b.append(createNamedQueriesOfAttr(displayAttribute, domainObjectName, pkAttribute));

		// Add named queries for unique key getter methods
		for (final DomainObject bean : domainObject.getFullInheritanceTree()) {
			if (bean.getDatabaseTable() == null)
				continue;

			for (final DBIndex key : bean.getDatabaseTable().getIndexes()) {
				if (!key.isUnique() || key.getColumns().size() > 2)
					continue;

				var getByName = "getBy";
				var findByName = "findBy";
				var checkName = "checkBy";
				var checkNameAndId = "checkBy";

				DomainAttribute uniqueAttribute1 = null;
				DomainAttribute uniqueAttribute2 = null;
				DomainAttribute uniqueAssocAttribute1 = null;
				AbstractDomainAssociation uniqueAssoc1 = null;
				DomainAttribute uniqueAssocAttribute2 = null;
				AbstractDomainAssociation uniqueAssoc2 = null;

				// Sort all attributes in order to be in sync with the respective repository!
				final var attributes = new BasicEList<DomainAttribute>();
				attributes.addAll(domainObject.getAllAttributes());

				ECollections.sort(attributes, new DomainAttributeComparator());

				// Get all attributes of the unique key
				for (final DBColumn column : key.getColumns())
					for (final DomainAttribute attribute : attributes)
						if (attribute.getColumn() != null && attribute.getColumn().equals(column)) {
							if (uniqueAttribute1 == null)
								uniqueAttribute1 = attribute;
							else
								uniqueAttribute2 = attribute;

							break;
						}

				// Sort all associations in order to be in sync with the respective repository!
				final var assocs = new BasicEList<AbstractDomainAssociation>();
				assocs.addAll(domainObject.getAllAssociations());

				ECollections.sort(assocs, new DomainAssociationComparator());

				for (final DBColumn column : key.getColumns())
					for (final AbstractDomainAssociation assoc : assocs)
						if (assoc instanceof final ManyToOneAssociation manyToOne && column.equals(manyToOne.getColumn())) {
							if (uniqueAssoc1 == null) {
								uniqueAssoc1 = manyToOne;
								uniqueAssocAttribute1 = manyToOne.getTarget().getPKAttribute();
							}
							else {
								uniqueAssoc2 = manyToOne;
								uniqueAssocAttribute2 = manyToOne.getTarget().getPKAttribute();
							}

							break;
						}
						else if (assoc instanceof final OneToOneAssociation oneToOne && oneToOne.isOwner()
								&& column.equals(oneToOne.getColumn())) {
							if (uniqueAssoc1 == null) {
								uniqueAssoc1 = oneToOne;
								uniqueAssocAttribute1 = oneToOne.getTarget().getPKAttribute();
							}
							else {
								uniqueAssoc2 = oneToOne;
								uniqueAssocAttribute2 = oneToOne.getTarget().getPKAttribute();
							}

							break;
						}

				int fieldCount = 0;

				if (uniqueAssoc1 != null)
					fieldCount++;

				if (uniqueAssoc2 != null)
					fieldCount++;

				if (uniqueAttribute1 != null)
					fieldCount++;

				if (uniqueAttribute2 != null)
					fieldCount++;

				// Omit the definition of the named query if the number of fields doesn't match the number of the unique key columns!
				if (fieldCount != key.getColumns().size())
					continue;

				if (uniqueAttribute1 != null && uniqueAttribute2 == null && uniqueAssoc1 == null)
					b.append(createNamedQueriesOfAttr(uniqueAttribute1, domainObjectName, pkAttribute));
				else if (uniqueAttribute1 != null && uniqueAttribute2 != null) {
					final var operator1 = uniqueAttribute1.getJavaType().isString() ? " like " : " = ";
					final var operator2 = uniqueAttribute2.getJavaType().isString() ? " like " : " = ";
					final String ukAttrName = uniqueAttribute1.getUpperCaseName() + "_And_" + uniqueAttribute2.getUpperCaseName();

					getByName += ukAttrName;
					findByName += ukAttrName;
					checkName += ukAttrName;
					checkNameAndId += ukAttrName + "_And_" + pkAttribute.getUpperCaseName();

					final var jpaQLFind = new StringBuilder();
					jpaQLFind.append("select a from " + domainObject.getName() + " a where a.");
					jpaQLFind.append(uniqueAttribute1.getName() + operator1 + ":");
					jpaQLFind.append(uniqueAttribute1.getName());
					jpaQLFind.append(" and a." + uniqueAttribute2.getName() + operator2 + ":");
					jpaQLFind.append(uniqueAttribute2.getName());

					final var jpaQLGet = new StringBuilder();
					jpaQLGet.append("select a from " + domainObject.getName() + " a where a." + uniqueAttribute1.getName() + " = :");
					jpaQLGet.append(uniqueAttribute1.getName());
					jpaQLGet.append(" and a." + uniqueAttribute2.getName() + " = :");
					jpaQLGet.append(uniqueAttribute2.getName());

					final var jpaQLCheck = new StringBuilder();
					jpaQLCheck.append("select count(a) from " + domainObject.getName());
					jpaQLCheck.append(" a where a." + uniqueAttribute1.getName() + " = :");
					jpaQLCheck.append(uniqueAttribute1.getName());
					jpaQLCheck.append(" and a." + uniqueAttribute2.getName() + " = :");
					jpaQLCheck.append(uniqueAttribute2.getName());

					final var jpaQLCheckByUniqueAndPrimaryKey = new StringBuilder();
					jpaQLCheckByUniqueAndPrimaryKey.append("select count(a) from " + domainObject.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" a where a." + uniqueAttribute1.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAttribute1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + uniqueAttribute2.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAttribute2.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + pkAttribute.getName() + " <> :");
					jpaQLCheckByUniqueAndPrimaryKey.append(pkAttribute.getName());

					// Add a named query to find the domain object by the unique key
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_FIND_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLGet.toString() + "\")\n");

					if (uniqueAttribute1.getJavaType().isString() || uniqueAttribute2.getJavaType().isString()) {
						// Add a named query to search domain objects by the unique key
						b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_SEARCH_BY_" + ukAttrName.toUpperCase());
						b.append(", query=\"" + jpaQLFind.toString() + "\")\n");

						namedQueryMap.put("NQ_UK_SEARCH_BY_" + ukAttrName.toUpperCase(), findByName);
					}

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLCheck.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_");
					b.append(pkAttribute.getName().toUpperCase() + ", query=\"" + jpaQLCheckByUniqueAndPrimaryKey.toString() + "\")\n");

					namedQueryMap.put("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase(), getByName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase(), checkName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_" + pkAttribute.getName().toUpperCase(),
							checkNameAndId);
				}
				else if (uniqueAttribute1 != null && uniqueAssoc1 != null) {
					final String ukAttrName = uniqueAttribute1.getUpperCaseName() + "_And_" + uniqueAssoc1.getUpperCaseName();

					getByName += ukAttrName;
					findByName += ukAttrName;
					checkName += ukAttrName;
					checkNameAndId += ukAttrName + "_And_" + pkAttribute.getUpperCaseName();

					final var jpaQLFind = new StringBuilder();
					jpaQLFind.append("select a from " + domainObject.getName() + " a where a.");
					jpaQLFind.append(uniqueAttribute1.getName() + " like :");
					jpaQLFind.append(uniqueAttribute1.getName());
					jpaQLFind.append(" and a." + uniqueAssoc1.getName() + "." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLFind.append(uniqueAssoc1.getName());

					final var jpaQLGet = new StringBuilder();
					jpaQLGet.append("select a from " + domainObject.getName() + " a where a." + uniqueAttribute1.getName() + " = :");
					jpaQLGet.append(uniqueAttribute1.getName());
					jpaQLGet.append(" and a." + uniqueAssoc1.getName() + "." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLGet.append(uniqueAssoc1.getName());

					final var jpaQLCheck = new StringBuilder();
					jpaQLCheck.append("select count(a) from " + domainObject.getName() + " a where a.");
					jpaQLCheck.append(uniqueAttribute1.getName() + " = :");
					jpaQLCheck.append(uniqueAttribute1.getName());
					jpaQLCheck.append(" and a." + uniqueAssoc1.getName() + "." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLCheck.append(uniqueAssoc1.getName());

					final var jpaQLCheckByUniqueAndPrimaryKey = new StringBuilder();
					jpaQLCheckByUniqueAndPrimaryKey.append("select count(a) from " + domainObject.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" a where a." + uniqueAttribute1.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAttribute1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + uniqueAssoc1.getName() + ".");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssocAttribute1.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + pkAttribute.getName() + " <> :");
					jpaQLCheckByUniqueAndPrimaryKey.append(pkAttribute.getName());

					final var jpaQLUKMap = new StringBuilder();
					jpaQLUKMap.append("select concat(concat(a." + uniqueAttribute1.getName() + ",';;;'),");
					jpaQLUKMap.append("a." + uniqueAssoc1.getName() + "." + uniqueAssocAttribute1.getName() + "),");
					jpaQLUKMap.append("a." + pkAttribute.getName() + " from " + domainObjectName + " a");

					// Add a named query to find the domain object by the unique key
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_FIND_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLGet.toString() + "\")\n");

					// Add a named query to search domain objects by the unique key
					if (uniqueAttribute1.getJavaType().isString()) {
						b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_SEARCH_BY_" + ukAttrName.toUpperCase());
						b.append(", query=\"" + jpaQLFind.toString() + "\")\n");

						namedQueryMap.put("NQ_UK_SEARCH_BY_" + ukAttrName.toUpperCase(), findByName);
					}

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLCheck.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_");
					b.append(pkAttribute.getName().toUpperCase() + ", query=\"" + jpaQLCheckByUniqueAndPrimaryKey.toString() + "\")\n");

					namedQueryMap.put("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase(), getByName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase(), checkName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_" + pkAttribute.getName().toUpperCase(),
							checkNameAndId);
				}
				else if (uniqueAttribute1 == null && uniqueAttribute2 == null && uniqueAssoc1 != null && uniqueAssoc2 != null) {
					final String ukAttrName = uniqueAssoc1.getUpperCaseName() + "_And_" + uniqueAssoc2.getUpperCaseName();

					getByName += ukAttrName;
					checkName += ukAttrName;
					checkNameAndId += ukAttrName + "_And_" + pkAttribute.getUpperCaseName();

					final var jpaQLGet = new StringBuilder();
					jpaQLGet.append("select a from " + domainObject.getName() + " a where a." + uniqueAssoc1.getName());
					jpaQLGet.append("." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLGet.append(uniqueAssoc1.getName());
					jpaQLGet.append(" and a." + uniqueAssoc2.getName() + "." + uniqueAssocAttribute2.getName() + " = :");
					jpaQLGet.append(uniqueAssoc2.getName());

					final var jpaQLCheck = new StringBuilder();
					jpaQLCheck.append("select count(a) from " + domainObject.getName() + " a where a." + uniqueAssoc1.getName());
					jpaQLCheck.append("." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLCheck.append(uniqueAssoc1.getName());
					jpaQLCheck.append(" and a." + uniqueAssoc2.getName() + "." + uniqueAssocAttribute2.getName() + " = :");
					jpaQLCheck.append(uniqueAssoc2.getName());

					final var jpaQLCheckByUniqueAndPrimaryKey = new StringBuilder();
					jpaQLCheckByUniqueAndPrimaryKey.append("select count(a) from " + domainObject.getName() + " a where a.");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append("." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + uniqueAssoc2.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append("." + uniqueAssocAttribute2.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc2.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + pkAttribute.getName() + " <> :");
					jpaQLCheckByUniqueAndPrimaryKey.append(pkAttribute.getName());

					final var jpaQLUKMap = new StringBuilder();
					jpaQLUKMap.append("select concat(concat(a." + uniqueAssoc1.getName());
					jpaQLUKMap.append("." + uniqueAssocAttribute1.getName() + ",';;;'),");
					jpaQLUKMap.append("a." + uniqueAssoc2.getName() + "." + uniqueAssocAttribute2.getName() + "),");
					jpaQLUKMap.append("a." + pkAttribute.getName() + " from " + domainObjectName + " a");

					// Add a named query to find the domain object by the unique key
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_FIND_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLGet.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLCheck.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_");
					b.append(pkAttribute.getName().toUpperCase() + ", query=\"" + jpaQLCheckByUniqueAndPrimaryKey.toString() + "\")\n");

					namedQueryMap.put("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase(), getByName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase(), checkName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_" + pkAttribute.getName().toUpperCase(),
							checkNameAndId);
				}
				else if (uniqueAttribute1 == null && uniqueAttribute2 == null && uniqueAssoc1 != null && uniqueAssoc2 == null) {
					final String ukAttrName = uniqueAssoc1.getUpperCaseName();

					getByName += ukAttrName;
					checkName += ukAttrName;
					checkNameAndId += ukAttrName + "_And_" + pkAttribute.getUpperCaseName();

					final var jpaQLGet = new StringBuilder();
					jpaQLGet.append("select a from " + domainObject.getName() + " a where a." + uniqueAssoc1.getName() + ".");
					jpaQLGet.append(uniqueAssocAttribute1.getName() + " = :" + uniqueAssoc1.getName());

					final var jpaQLCheck = new StringBuilder();
					jpaQLCheck.append("select count(a) from " + domainObject.getName());
					jpaQLCheck.append(" a where a." + uniqueAssoc1.getName() + ".");
					jpaQLCheck.append(uniqueAssocAttribute1.getName() + " = :" + uniqueAssoc1.getName());

					final var jpaQLCheckByUniqueAndPrimaryKey = new StringBuilder();
					jpaQLCheckByUniqueAndPrimaryKey.append("select count(a) from " + domainObject.getName() + " a where a.");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append("." + uniqueAssocAttribute1.getName() + " = :");
					jpaQLCheckByUniqueAndPrimaryKey.append(uniqueAssoc1.getName());
					jpaQLCheckByUniqueAndPrimaryKey.append(" and a." + pkAttribute.getName() + " <> :" + pkAttribute.getName());

					final var jpaQLUKMap = new StringBuilder();
					jpaQLUKMap.append("select a." + uniqueAssoc1.getName() + "." + uniqueAssocAttribute1.getName() + ",");
					jpaQLUKMap.append("a." + pkAttribute.getName() + " from " + domainObjectName + " a");

					// Add a named query to find the domain object by the unique key
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_FIND_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLGet.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
					b.append(", query=\"" + jpaQLCheck.toString() + "\")\n");

					// Add a named query to check if the domain object already exists
					b.append("@NamedQuery(name=" + domainObjectName + ".NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase());
					b.append("_AND_" + pkAttribute.getName().toUpperCase());
					b.append(", query=\"" + jpaQLCheckByUniqueAndPrimaryKey.toString() + "\")\n");

					namedQueryMap.put("NQ_UK_FIND_BY_" + ukAttrName.toUpperCase(), getByName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase(), checkName);
					namedQueryMap.put("NQ_UK_EXISTS_BY_" + ukAttrName.toUpperCase() + "_AND_" + pkAttribute.getName().toUpperCase(),
							checkNameAndId);
				}
			}
		}

		// Add named queries for lazy resolvers
		domainObject.getAllAssociations().forEach(association -> {
			var name = "get" + association.getUpperCaseName();

			// Add a named query to get the referenced domain object by its ID
			b.append("@NamedQuery(name=" + domainObjectName + ".NQ_GET_" + association.getName().toUpperCase());
			b.append(", query=\"select b from " + domainObjectName);
			b.append(" a join a." + association.getName() + " b where a.");
			b.append(pkAttribute.getName() + " = :" + pkAttribute.getName() + "\")\n");

			namedQueryMap.put("NQ_GET_" + association.getName().toUpperCase(), name);

			if (association instanceof final OneToManyAssociation otm && !otm.isBidirectional()) {
				final var targetPKName = association.getTarget().getPKAttribute().getName();
				name = "getBy" + association.getUpperCaseName();

				// Add a named query to get the domain object by one of its referenced objects
				b.append("@NamedQuery(name=" + domainObjectName + ".NQ_GET_BY_" + association.getName().toUpperCase());
				b.append(", query=\"select a from " + domainObjectName);
				b.append(" a join a." + association.getName() + " b where b.");
				b.append(targetPKName + " = :" + targetPKName + "\")\n");

				namedQueryMap.put("NQ_GET_BY_" + association.getName().toUpperCase(), name);
			}
		});

		// Add a named query to delete all domain objects
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_DELETE_ALL, query=\"delete from " + domainObjectName + " a\")\n");

		// Add a named query to delete the domain object
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_DELETE, query=\"delete from " + domainObjectName);
		b.append(" a where a." + pkAttribute.getName() + " = :" + pkAttribute.getName() + "\")\n");

		// Add a named query to get all domain objects
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_GET_ALL, query=\"select a from " + domainObjectName + " a\")\n");

		// Add a named query to get the domain object by its ID
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_FIND, query=\"select a from " + domainObjectName);
		b.append(" a where a." + pkAttribute.getName() + " = :" + pkAttribute.getName() + "\")\n");

		// Add a named query to test if a domain object already exists
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_CHECK, query=\"select count(a) from " + domainObjectName);
		b.append(" a where a." + pkAttribute.getName() + " = :" + pkAttribute.getName() + "\")\n");

		// Add a named query to count all domain objects
		b.append("@NamedQuery(name=" + domainObjectName + ".NQ_COUNT, query=\"select count(a) from " + domainObjectName + " a\")\n");

		namedQueryMap.put("NQ_DELETE_ALL", "deleteAll");
		namedQueryMap.put("NQ_GET_ALL", "getAll");
		namedQueryMap.put("NQ_FIND", "find");
		namedQueryMap.put("NQ_COUNT", "count");
		namedQueryMap.put("NQ_CHECK", "check");
		namedQueryMap.put("NQ_DELETE", "delete");

		return b.toString();
	}

	/**
	 * @param attr
	 * @return the JPA annotations for a given attribute
	 */
	private String addPersistenceAnnotations(DomainAttribute attr) {
		final var b = new StringBuilder();
		final DomainObject domainObjectOfAttr = attr.getDomainObject();

		if (attr.isPk()) {
			b.append("@Id\n");

			if (domainObjectOfAttr.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.SEQUENCE
					|| domainObjectOfAttr.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.TABLE) {
				b.append("@GeneratedValue(strategy=GenerationType." + domainObjectOfAttr.getIDGenerator().getGeneratorType().toString());
				b.append(", generator=\"" + generatorName + "\")\n");
			}
			else if (domainObjectOfAttr.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.UUID) {
				if (project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
					importClass("org.hibernate.annotations.UuidGenerator");

					b.append("@UuidGenerator\n");
				}
				else {
					importClass("org.eclipse.persistence.annotations.UuidGenerator");

					b.append("@UuidGenerator(name=\"" + generatorName + "\")\n");
				}

				b.append("@GeneratedValue(generator=\"" + generatorName + "\")\n");
			}
			else if (domainObjectOfAttr.getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE) {
				b.append("@GeneratedValue(strategy=GenerationType.");
				b.append(domainObjectOfAttr.getIDGenerator().getGeneratorType().toString() + ")\n");
			}
		}

		if (attr.isTrackVersion())
			b.append("@Version\n");

		if (attr.getJavaType().isEnum())
			b.append("@Enumerated(EnumType.STRING)\n");

		if (attr.isPersistent()) {
			if (attr.isPk())
				b.append("@Column(name=\"" + attr.getColumn().getMappingName() + "\"");
			else {
				if (!attr.getColumn().isNullable() || !attr.isFetchTypeEager()) {
					b.append("@Basic(");

					if (!attr.getColumn().isNullable())
						b.append("optional=false");

					if (!attr.isFetchTypeEager()) {
						if (!attr.getColumn().isNullable())
							b.append(", ");

						b.append("fetch=FetchType.LAZY");
					}

					b.append(")\n");
				}

				b.append("@Column(name=\"" + attr.getColumn().getMappingName() + "\", nullable=" + attr.getColumn().isNullable());
				b.append(", updatable=" + attr.isUpdatable() + ", insertable=" + attr.isInsertable());

				if (attr.getJavaType().isString() && attr.getDomainAttributeValidator().getMaxLength() != null)
					b.append(", length=" + attr.getDomainAttributeValidator().getMaxLength());

				if (attr.getColumn().getPrecision() != 0)
					b.append(", precision=" + attr.getColumn().getPrecision());

				if (attr.getColumn().getScale() != 0)
					b.append(", scale=" + attr.getColumn().getScale());

				DBTable table = domainObjectOfAttr.getDatabaseTable();

				if (table == null)
					table = domainObjectOfAttr.getRootParentDomainObject(false).getDatabaseTable();

				if (table != null)
					for (final DBIndex index : table.getIndexes()) {
						if (!index.isUnique())
							continue;

						if (index.getColumns().size() == 1 && index.getColumns().contains(attr.getColumn())) {
							b.append(", unique=true");
							break;
						}
					}
			}

			b.append(")\n");

			if (attr.getTemporalType() != TemporalTypeEnumeration.NONE) {
				final PersistenceProviderEnumeration provider = project.getPersistenceProvider();
				boolean addTemporalType = true;

				// The @Temporal annotation for GregorianCalendar fields is mandatory when using EclipseLink. But Hibernate will throw an
				// exception if a GregorianCalendar field is annotated with @Temporal!
				if (attr.getJavaType().isCalendar() && provider == PersistenceProviderEnumeration.HIBERNATE)
					addTemporalType = false;

				if (addTemporalType)
					b.append("@Temporal(value=TemporalType." + attr.getTemporalType().toString() + ")\n");
			}

			if (attr.isLob()) {
				b.append("@Lob\n");

				// A field that is mapped to a PostgreSQL column of type 'bytea' requires an additional Hibernate-specific annotation!
				if (project.getDatabase().getVendorGroup() == DBVendorGroupEnumeration.POSTGRESQL
						&& project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
					importClass("org.hibernate.annotations.JdbcTypeCode");
					importClass("org.hibernate.type.SqlTypes");

					b.append("@JdbcTypeCode(SqlTypes.BINARY)\n");
				}
			}
			else if (attr.getJavaType().isUUID() && !attr.getColumn().getColumnType().getName().equalsIgnoreCase(JavaType.UUID)) {
				if (project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE) {
					final var jdbcType = attr.isWildcardFilteringSupported() ? "VARCHAR" : "BINARY";

					importClass("org.hibernate.annotations.JdbcTypeCode");
					importClass("org.hibernate.type.SqlTypes");

					// It might be necessary to change the selected default type manually!
					b.append("@JdbcTypeCode(SqlTypes." + jdbcType + ")\n");
				}
				else {
					importPackage("net.codecadenza.runtime.jpa.converter");

					if (attr.isWildcardFilteringSupported())
						b.append("@Convert(converter=UUIDStringConverter.class)\n");
					else
						b.append("@Convert(converter=UUIDByteArrayConverter.class)\n");
				}
			}
		}
		else
			b.append("@Transient\n");

		return b.toString();
	}

	/**
	 * Add the validation annotations for a domain attribute
	 * @param attr
	 * @return the generated content
	 */
	private String addValidationAnnotations(DomainAttribute attr) {
		final var b = new StringBuilder();

		if (!attr.isPersistent())
			return "";

		if (!attr.isPk()) {
			if (!attr.isTrackVersion() && !attr.getDomainAttributeValidator().isNullable() && !attr.getJavaType().isPrimitive())
				b.append("@NotNull(message=\"Field \\\"" + attr.getName() + "\\\" must not be null!\")\n");
		}
		else if (!attr.getDomainAttributeValidator().isNullable()
				&& attr.getDomainObject().getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.NONE)
			b.append("@NotNull(message=\"Field \\\"" + attr.getName() + "\\\" must not be null!\")\n");

		// Constraints concerning length and regular expressions make sense for String attributes only!
		if (attr.getJavaType().isString()) {
			final Optional<Integer> minLength = attr.getMinFieldLength();
			final Optional<Integer> maxLength = attr.getMaxFieldLenght();

			if (useInternalValidation) {
				if (minLength.isPresent()) {
					b.append("@MinLength(value=" + minLength.get());
					b.append(", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a minimal length of " + minLength.get() + "!\")\n");
				}

				if (maxLength.isPresent()) {
					b.append("@MaxLength(value=" + maxLength.get());
					b.append(", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a maximal length of " + maxLength.get() + "!\")\n");
				}
			}
			else if (minLength.isPresent() || maxLength.isPresent()) {
				boolean isMinSet = false;

				b.append("@Size(");

				if (minLength.isPresent()) {
					b.append("min=" + minLength.get());

					isMinSet = true;
				}

				if (maxLength.isPresent()) {
					if (isMinSet)
						b.append(", ");

					b.append("max=" + maxLength.get());
				}

				b.append(", message=\"Length of field \\\"" + attr.getName() + "\\\" is illegal!\")\n");
			}

			if (!attr.getDomainAttributeValidator().getRegularExpression().isEmpty()) {
				var regExAnnotation = "@RegularExpression(value";

				if (!useInternalValidation)
					regExAnnotation = "@Pattern(regexp";

				b.append(regExAnnotation + "=\"" + attr.getDomainAttributeValidator().getRegularExpression());
				b.append("\", message=\"Field \\\"" + attr.getName());
				b.append("\\\" must match regular expression \\\"" + attr.getDomainAttributeValidator().getRegularExpression());
				b.append("\\\"!\")\n");
			}
		}

		if (attr.getDomainAttributeValidator().isPastDate()) {
			var regExAnnotation = "@PastDate";

			if (!useInternalValidation)
				regExAnnotation = "@Past";

			b.append(regExAnnotation + "(message=\"Field \\\"" + attr.getName() + "\\\" must contain a past date!\")\n");
		}

		if (attr.getDomainAttributeValidator().isFutureDate()) {
			var regExAnnotation = "@FutureDate";

			if (!useInternalValidation)
				regExAnnotation = "@Future";

			b.append(regExAnnotation + "(message=\"Field \\\"" + attr.getName() + "\\\" must contain a future date!\")\n");
		}

		if (!attr.getDomainAttributeValidator().getMinValue().isEmpty()) {
			final JavaType type = attr.getJavaType();
			final String minValue = attr.getDomainAttributeValidator().getMinValue();

			if (useInternalValidation) {
				if (type.isIntegerOrLong()) {
					b.append("@MinIntegerValue(value=" + minValue + ", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a minimum value of " + minValue + "!\")\n");
				}
				else if (type.isType(JavaType.DOUBLE, JavaType.DOUBLE_OBJ, JavaType.FLOAT, JavaType.FLOAT_OBJ)) {
					b.append("@MinFloatValue(value=" + minValue + ", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a minimum value of " + minValue + "!\")\n");
				}
				else if (type.isBigDecimal()) {
					b.append("@MinDecimalValue(value=\"" + minValue + "\", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a minimum value of " + minValue + "!\")\n");
				}
			}
			else if (type.isIntegerOrLong()) {
				b.append("@Min(value=" + minValue + ", message=\"Field \\\"" + attr.getName());
				b.append("\\\" must have a minimum value of " + minValue + "!\")\n");
			}
			else if (type.isBigDecimal()) {
				b.append("@DecimalMin(value=\"" + minValue + "\", message=\"Field \\\"" + attr.getName());
				b.append("\\\" must have a minimum value of " + minValue + "!\")\n");
			}
		}

		if (!attr.getDomainAttributeValidator().getMaxValue().isEmpty()) {
			final JavaType type = attr.getJavaType();
			final String maxValue = attr.getDomainAttributeValidator().getMaxValue();

			if (useInternalValidation) {
				if (type.isIntegerOrLong()) {
					b.append("@MaxIntegerValue(value=" + maxValue + ", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a maximum value of " + maxValue + "!\")\n");
				}
				else if (type.isType(JavaType.DOUBLE, JavaType.DOUBLE_OBJ, JavaType.FLOAT, JavaType.FLOAT_OBJ)) {
					b.append("@MaxFloatValue(value=" + maxValue + ", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a maximum value of " + maxValue + "!\")\n");
				}
				else if (type.isBigDecimal()) {
					b.append("@MaxDecimalValue(value=\"" + maxValue + "\", message=\"Field \\\"" + attr.getName());
					b.append("\\\" must have a maximum value of " + maxValue + "!\")\n");
				}
			}
			else if (type.isIntegerOrLong()) {
				b.append("@Max(value=" + maxValue + ", message=\"Field \\\"" + attr.getName());
				b.append("\\\" must have a maximum value of " + maxValue + "!\")\n");
			}
			else if (type.isBigDecimal()) {
				b.append("@DecimalMax(value=\"" + maxValue + "\", message=\"Field \\\"" + attr.getName());
				b.append("\\\" must have a maximum value of " + maxValue + "!\")\n");
			}
		}

		return b.toString();
	}

	/**
	 * @param table
	 * @return the generated content
	 */
	private String addSchemaAndCatalogAnnotationParams(DBTable table) {
		final var b = new StringBuilder();

		if (table.getDatabase() == null)
			return b.toString();

		final String defaultSchemaName = table.getDatabase().getSchemaName();
		final String defaultCatalogName = table.getDatabase().getCatalogName();
		var tableSchemaName = "";
		var tableCatalogName = "";

		if (table.getSchemaName() != null && !table.getSchemaName().isEmpty() && !table.getSchemaName().equals(defaultSchemaName))
			tableSchemaName = table.getSchemaName();

		if (!tableSchemaName.isEmpty())
			b.append(", schema=\"" + DBNamingUtil.convertToMapping(tableSchemaName, table.getDatabase()) + "\"");

		if (table.getCatalogName() != null && !table.getCatalogName().isEmpty() && !table.getCatalogName().equals(defaultCatalogName))
			tableCatalogName = table.getCatalogName();

		if (!tableCatalogName.isEmpty())
			b.append(", catalog=\"" + DBNamingUtil.convertToMapping(tableCatalogName, table.getDatabase()) + "\"");

		return b.toString();
	}

	/**
	 * Add the validation annotation for a domain association
	 * @param assoc
	 * @return the generated content
	 */
	private String addValidationAnnotation(AbstractDomainAssociation assoc) {
		final var b = new StringBuilder();

		if ((assoc instanceof final ManyToOneAssociation mto && !mto.isOptional())
				|| (assoc instanceof final OneToOneAssociation oto && !oto.isOptional() && oto.isOwner()))
			b.append("@NotNull(message=\"Field \\\"" + assoc.getName() + "\\\" must not be null!\")\n");

		return b.toString();
	}

	/**
	 * Add the one-to-many annotation
	 * @param oneToManyAssociation
	 * @return the generated content
	 */
	private String addOneToManyAnnotation(OneToManyAssociation oneToManyAssociation) {
		final var b = new StringBuilder();

		if (oneToManyAssociation.isBidirectional()) {
			b.append("@OneToMany(targetEntity = " + oneToManyAssociation.getTarget().getName());
			b.append(".class, mappedBy = \"" + oneToManyAssociation.getReverseAssociation().getName() + "\", ");
		}
		else
			b.append("@OneToMany(");

		if (oneToManyAssociation.isFetchTypeEager())
			b.append("fetch=FetchType.EAGER");
		else
			b.append("fetch=FetchType.LAZY");

		b.append(addCascadeOperations(oneToManyAssociation.isCascadeMerge(), oneToManyAssociation.isCascadePersist(),
				oneToManyAssociation.isCascadeRefresh(), oneToManyAssociation.isCascadeRemove()));
		b.append(")\n");

		if (!oneToManyAssociation.isBidirectional()) {
			final DBTable associationTable = oneToManyAssociation.getTable();
			// It is assumed that the association table has at least two columns with the correct order!
			final DBColumn joinColumn = associationTable.getColumns().get(0);
			final DBColumn inverseJoinColumn = associationTable.getColumns().get(1);

			b.append("@JoinTable(name=\"" + associationTable.getMappingName() + "\"");
			b.append(addSchemaAndCatalogAnnotationParams(associationTable));
			b.append(", joinColumns={@JoinColumn(name=\"");
			b.append(joinColumn.getMappingName() + "\")}, inverseJoinColumns={@JoinColumn(name=\"");
			b.append(inverseJoinColumn.getMappingName() + "\")})\n");
		}

		return b.toString();
	}

	/**
	 * Add the many-to-one annotation
	 * @param manyToOneAssociation
	 * @return the generated content
	 */
	private String addManyToOneAnnotation(ManyToOneAssociation manyToOneAssociation) {
		final var b = new StringBuilder();
		DBTable targetTable = manyToOneAssociation.getTarget().getDatabaseTable();

		if (targetTable == null)
			targetTable = manyToOneAssociation.getTarget().getRootParentDomainObject(false).getDatabaseTable();

		b.append("@ManyToOne(");

		if (manyToOneAssociation.isFetchTypeEager())
			b.append("fetch=FetchType.EAGER");
		else
			b.append("fetch=FetchType.LAZY");

		b.append(", optional=" + manyToOneAssociation.isOptional());

		if (manyToOneAssociation.isOwner())
			b.append(addCascadeOperations(manyToOneAssociation.isCascadeMerge(), manyToOneAssociation.isCascadePersist(),
					manyToOneAssociation.isCascadeRefresh(), manyToOneAssociation.isCascadeRemove()));

		b.append(")\n");
		b.append("@JoinColumn(name=\"" + manyToOneAssociation.getColumn().getMappingName() + "\"");
		b.append(", referencedColumnName=\"");
		b.append(targetTable.getPrimaryKey().getColumn().getMappingName() + "\"");
		b.append(", nullable=" + manyToOneAssociation.isOptional() + ")\n");

		return b.toString();
	}

	/**
	 * Add the one-to-one annotation
	 * @param oneToOneAssociation
	 * @return the generated content
	 */
	private String addOneToOneAnnotation(OneToOneAssociation oneToOneAssociation) {
		final var b = new StringBuilder();
		b.append("@OneToOne(fetch=FetchType.");

		if (oneToOneAssociation.isFetchTypeEager())
			b.append("EAGER");
		else
			b.append("LAZY");

		if (oneToOneAssociation.isOwner()) {
			DBTable targetTable = oneToOneAssociation.getTarget().getDatabaseTable();

			if (targetTable == null)
				targetTable = oneToOneAssociation.getTarget().getRootParentDomainObject(false).getDatabaseTable();

			b.append(", optional=" + oneToOneAssociation.isOptional());
			b.append(addCascadeOperations(oneToOneAssociation.isCascadeMerge(), oneToOneAssociation.isCascadePersist(),
					oneToOneAssociation.isCascadeRefresh(), oneToOneAssociation.isCascadeRemove()));
			b.append(")\n");
			b.append("@JoinColumn(name=\"" + oneToOneAssociation.getColumn().getMappingName() + "\"");
			b.append(", referencedColumnName=\"");
			b.append(targetTable.getPrimaryKey().getColumn().getMappingName() + "\"");
			b.append(", nullable=" + oneToOneAssociation.isOptional() + ")\n");
		}
		else
			b.append(", mappedBy=\"" + oneToOneAssociation.getReverseAssociation().getName() + "\")\n");

		return b.toString();
	}

	/**
	 * Add the many-to-many annotation
	 * @param manyToManyAssociation
	 * @return the generated content
	 */
	private String addManyToManyAnnotation(ManyToManyAssociation manyToManyAssociation) {
		final var b = new StringBuilder();
		b.append("@ManyToMany(");

		if (!manyToManyAssociation.isOwner())
			b.append("mappedBy=\"" + manyToManyAssociation.getReverseAssociation().getName() + "\", ");

		if (manyToManyAssociation.isFetchTypeEager())
			b.append("fetch=FetchType.EAGER");
		else
			b.append("fetch=FetchType.LAZY");

		if (manyToManyAssociation.isOwner())
			b.append(addCascadeOperations(manyToManyAssociation.isCascadeMerge(), manyToManyAssociation.isCascadePersist(),
					manyToManyAssociation.isCascadeRefresh(), manyToManyAssociation.isCascadeRemove()));

		b.append(")\n");

		if (manyToManyAssociation.isOwner()) {
			final DBTable associationTable = manyToManyAssociation.getTable();
			// It is assumed that the association table has at least two columns with the correct order!
			final DBColumn joinColumn = associationTable.getColumns().get(0);
			final DBColumn inverseJoinColumn = associationTable.getColumns().get(1);

			b.append("@JoinTable(name=\"" + associationTable.getMappingName() + "\"");
			b.append(addSchemaAndCatalogAnnotationParams(associationTable));
			b.append(", joinColumns={@JoinColumn(name=\"");
			b.append(joinColumn.getMappingName() + "\")}, inverseJoinColumns={@JoinColumn(name=\"");
			b.append(inverseJoinColumn.getMappingName() + "\")})\n");
		}

		return b.toString();
	}

	/**
	 * Add the cascade operations to a given JPA association annotation
	 * @param merge
	 * @param persist
	 * @param refresh
	 * @param remove
	 * @return the generated content
	 */
	private String addCascadeOperations(boolean merge, boolean persist, boolean refresh, boolean remove) {
		final var b = new StringBuilder();

		if (merge && persist && refresh && remove)
			b.append(", cascade={ CascadeType.ALL }");
		else if (merge || persist || refresh || remove) {
			boolean separator = false;
			b.append(", cascade={ ");

			if (merge) {
				b.append("CascadeType.MERGE");
				separator = true;
			}

			if (persist) {
				if (separator)
					b.append(", ");

				b.append("CascadeType.PERSIST");
				separator = true;
			}

			if (refresh) {
				if (separator)
					b.append(", ");

				b.append("CascadeType.REFRESH");
				separator = true;
			}

			if (remove) {
				if (separator)
					b.append(", ");

				b.append("CascadeType.REMOVE");
			}

			b.append("}");
		}

		return b.toString();
	}

}
