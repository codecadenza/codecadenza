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
package net.codecadenza.eclipse.generator.exchange.method;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_ROOT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_PATH_PARAM;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_UTIL;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE_ENTITY;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_GET_REFERENCE;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Abstract base class for data import method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractImportMethodGenerator extends AbstractExchangeMethodGenerator {
	protected static final String DATA_IMPORT_LIST = "dataImportObjList";
	protected static final String PARSER_METHOD_PREFIX = "parse";
	protected static final String FINDER_METHOD_PREFIX = "find";
	protected static final String PARSER_METHOD_SUFFIX = "Data";
	protected static final String CONTR_METHOD_PREFIX_FIND = "find";
	protected static final String CONTR_METHOD_PREFIX_CREATE = "create";
	protected static final String CONTR_METHOD_PREFIX_FIND_CREATE = "findOrCreate";

	protected HashSet<AbstractDomainAssociation> controlledAssocSetNew = new HashSet<>();
	protected HashSet<AbstractDomainAssociation> controlledAssocSetUpdate = new HashSet<>();
	private boolean handleMultipleFiles;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	protected AbstractImportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);

		if (fileExchangeMode != null && !method.hasPathParameter()
				&& (fileExchangeMode.getFileNamePattern() == null || fileExchangeMode.getFileNamePattern().isEmpty()))
			handleMultipleFiles = true;
	}

	/**
	 * @return the generated content
	 */
	protected abstract String addUnmarshallerInvocation();

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {
		if (usesCustomFinderMethod())
			addCustomFinderMethod();

		method.getRootElement().getAllElements().forEach(subElement -> {
			addOneToManyMethods(subElement);
			addOneToOneMethods(subElement);
		});

		createAssociationControllerMethods();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodComment()
	 */
	@Override
	public String createMethodComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));
		b.append(" * @throws DataImportException if import operation has failed\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * @param contr
	 * @return the root mapping object for the given association controller
	 */
	protected ExchangeMappingObject getControllerRootMappingObject(AssociationController contr) {
		final var mappingMap = new HashMap<ExchangeMappingObject, Integer>();
		final var contrAttributes = new BasicEList<ExchangeMappingAttribute>();

		contrAttributes.addAll(contr.getPersistAttributes());
		contrAttributes.addAll(contr.getQueryAttributes());

		// At least one mapping attribute must be defined in order to find a referenced object!
		if (contrAttributes.isEmpty())
			return null;

		// A controller can contain any number of attributes that in turn belong to different mapping objects!
		contrAttributes.forEach(attr -> {
			if (mappingMap.containsKey(attr.getExchangeMappingObject())) {
				// If two attributes of the same mapping object exist we have to change the level accordingly!
				final int existingLevel = mappingMap.get(attr.getExchangeMappingObject());
				final int newLevel = attr.getAssociationList().size();

				if (newLevel < existingLevel)
					mappingMap.put(attr.getExchangeMappingObject(), newLevel);
			}
			else
				mappingMap.put(attr.getExchangeMappingObject(), attr.getAssociationList().size());
		});

		final List<Map.Entry<ExchangeMappingObject, Integer>> mappingObjectList = new LinkedList<>(mappingMap.entrySet());

		// We must sort all mapping objects based on their association depth!
		mappingObjectList.sort((obj1, obj2) -> obj1.getValue().compareTo(obj2.getValue()));

		// We pick up the mapping object of the highest level!
		final ExchangeMappingObject rootMappingObject = mappingObjectList.get(0).getKey();

		// We don't support two different mapping objects on the root level!
		if (mappingObjectList.size() > 1 && !mappingObjectList.get(1).getKey().equals(rootMappingObject)
				&& mappingObjectList.get(0).getValue().equals(mappingObjectList.get(1).getValue()))
			return null;

		return rootMappingObject;
	}

	/**
	 * Create additional methods that overwrite the default association control
	 */
	protected void createAssociationControllerMethods() {
		method.getAssociationControllers().forEach(contr -> {
			final ExchangeMappingObject rootMappingObject = getControllerRootMappingObject(contr);

			if (rootMappingObject != null)
				addAssociationControllerMethod(contr, rootMappingObject);
		});
	}

	/**
	 * Search for association controller by given association
	 * @param assoc
	 * @return the respective association controller
	 */
	protected AssociationController getAssociationController(AbstractDomainAssociation assoc) {
		if (assoc == null)
			return null;

		return method.getAssociationControllers().stream()
				.filter(contr -> contr.getAssociation().equals(assoc) && getControllerRootMappingObject(contr) != null).findFirst()
				.orElse(null);
	}

	/**
	 * Check if an attribute exists that is mapped to a primary key attribute which controls this association
	 * @param assoc
	 * @return true if the association is driven by a primary key attribute
	 */
	protected boolean isAssociationDrivenByPrimaryKey(AbstractDomainAssociation assoc) {
		for (final DataExchangeElement element : method.getRootElement(true).getAllElements()) {
			if (element.getMappingObject() == null)
				continue;

			final ExchangeMappingObject mappingObject = element.getMappingObject();

			for (final MappingAttribute attr : mappingObject.getAttributes())
				if (attr.getAssociation() != null && attr.getAssociation().equals(assoc) && attr.getDomainAttribute() != null
						&& attr.getDomainAttribute().isPk())
					return true;
		}

		return false;
	}

	/**
	 * @param attr
	 * @param rootMappingObject
	 * @return the generated getter fragment
	 */
	protected String createMappingAttributeGetter(ExchangeMappingAttribute attr, ExchangeMappingObject rootMappingObject) {
		ExchangeMappingObject mappingObject = attr.getExchangeMappingObject();
		final EList<AbstractDomainAssociation> assocList = attr.getAssociationList();
		var getter = "";

		if (mappingObject.equals(rootMappingObject)) {
			getter = "." + attr.getGetterName();
			return getter;
		}

		DataExchangeElement element = attr.getDataExchangeElement(false);

		if (!element.isContainer())
			element = element.getParentElement();

		while (true) {
			mappingObject = element.getMappingObject();

			for (final ExchangeMappingAttribute a : mappingObject.getAttributes()) {
				// We only iterate over attributes that represent a container!
				if (a.getDomainAttribute() != null || a.getAssociation() == null)
					continue;

				if (assocList != null)
					for (final AbstractDomainAssociation assoc : assocList)
						if (a.getAssociation().equals(assoc)) {
							if (!getter.isEmpty())
								getter = "." + getter;

							getter = a.getGetterName() + getter;
							break;
						}

				if (a.getAssociation().equals(attr.getAssociation())) {
					if (!getter.isEmpty())
						getter = "." + getter;

					getter = a.getGetterName() + getter;
				}
			}

			if (mappingObject.equals(rootMappingObject)) {
				getter += "." + attr.getGetterName();
				break;
			}

			element = element.getParentElement();
		}

		return "." + getter;
	}

	/**
	 * @param attr
	 * @param parentAssoc
	 * @param targetDomainObjectName
	 * @param updateMode
	 * @return the generated content
	 */
	protected String createDomainAssocIfStatement(ExchangeMappingAttribute attr, AbstractDomainAssociation parentAssoc,
			String targetDomainObjectName, boolean updateMode) {
		final var ifFragment = new StringBuilder();
		final var getterFragment = new StringBuilder();
		final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(attr, parentAssoc);

		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);
			boolean optional = false;

			if (ia instanceof final ManyToOneAssociation mto)
				optional = mto.isOptional();

			if (ia instanceof final OneToOneAssociation oto)
				optional = oto.isOptional() && updateMode;

			if (!getterFragment.isEmpty())
				getterFragment.append(".");

			getterFragment.append(ia.getGetterName());

			if (optional)
				ifFragment.append("if(" + targetDomainObjectName + "." + getterFragment.toString() + " != null)\n");
		}

		return ifFragment.toString();
	}

	/**
	 * @param contr
	 * @param domainObject
	 * @param rootMappingObject
	 * @return the generated content
	 */
	protected String createQueryFragment(AssociationController contr, DomainObject domainObject,
			ExchangeMappingObject rootMappingObject) {
		final var b = new StringBuilder();
		final String targetDomainObjectName = domainObject.getLowerCaseName();

		if (generator != null)
			generator.importPackage(PACK_JAVA_UTIL);

		// Create the query statement
		b.append("final var queryStatement = \"select a from " + domainObject.getName() + " a where ");

		boolean firstAttr = true;
		int paramCount = 1;

		for (final ExchangeMappingAttribute attr : contr.getQueryAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (firstAttr)
				firstAttr = false;
			else
				b.append(" and ");

			b.append("a.");

			if (attr.getAssociation() != null) {
				final AbstractDomainAssociation assoc = attr.getAssociation();

				if (contr.getAssociation() instanceof ManyToOneAssociation) {
					if (!assoc.equals(contr.getAssociation()) && !attr.getAssociationList().isEmpty()
							&& attr.getAssociationList().get(0).equals(contr.getAssociation()))
						b.append(assoc.getName() + ".");
				}
				else
					b.append(assoc.getName() + ".");
			}

			b.append(attr.getDomainAttribute().getName() + " = :param" + paramCount++);
		}

		b.append("\";\n");
		b.append("final var parameterMap = new HashMap<String, Object>();\n\n");

		paramCount = 1;

		// Set all query parameters
		for (final ExchangeMappingAttribute attr : contr.getQueryAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			final String mappingGetter = createMappingAttributeGetter(attr, rootMappingObject);

			b.append("parameterMap.put(\"param" + paramCount++ + "\", " + DEFAULT_MAPPING_OBJ_NAME + mappingGetter + ");\n");
		}

		b.append("\n");

		final RepositoryMethod searchMethod = domainObject.findRepositoryMethod(RepositoryMethodTypeEnumeration.SEARCH);

		if (searchMethod == null)
			return createOutputForMissingRepositoryMethod(domainObject, RepositoryMethodTypeEnumeration.SEARCH);

		final Repository repository = searchMethod.getRepository();
		final String repositoryName = createRepositoryName(repository.getDomainObject());

		repositories.put(repositoryName, repository);

		b.append("// We actually search for only one object but we want to know if the query really returns a unique result!\n");
		b.append("final List<" + domainObject.getName() + "> " + DEFAULT_OBJECT_LIST_NAME + " = ");
		b.append(repositoryName + "." + searchMethod.getName() + "(queryStatement, 2, 0, parameterMap);\n\n");
		b.append("if(" + DEFAULT_OBJECT_LIST_NAME + ".size() == 1)\n");
		b.append(targetDomainObjectName + " = " + DEFAULT_OBJECT_LIST_NAME + ".get(0);\n\n");
		b.append("if(" + DEFAULT_OBJECT_LIST_NAME + ".size() > 1)\n");
		b.append("throw new IllegalStateException(\"Query to find association \\\"");
		b.append(contr.getAssociation().getName() + "\\\" returned non-unique result!\");\n\n");

		return b.toString();
	}

	/**
	 * @param contr
	 * @param domainObject
	 * @param rootMappingObject
	 * @return the generated content
	 */
	protected String createFragmentForMissingRef(AssociationController contr, DomainObject domainObject,
			ExchangeMappingObject rootMappingObject) {
		final var b = new StringBuilder();
		final boolean find = !contr.getQueryAttributes().isEmpty();
		final String targetDomainObjectName = domainObject.getLowerCaseName();

		if (generator != null)
			generator.importPackage(domainObject.getNamespace().toString());

		if (find) {
			b.append("if(" + targetDomainObjectName + " == null)\n");
			b.append("{\n");
		}
		else
			b.append("var ");

		b.append(targetDomainObjectName + " = new " + domainObject.getName() + "();\n");

		final var mtoSet = new HashSet<AbstractDomainAssociation>();

		for (final ExchangeMappingAttribute attr : contr.getPersistAttributes()) {
			final String getter = createMappingAttributeGetter(attr, rootMappingObject);
			final DomainAttribute domainAttr = attr.getDomainAttribute();
			boolean findRef = false;

			if (contr.getAssociation() instanceof ManyToOneAssociation && !attr.getAssociationList().isEmpty())
				findRef = true;

			if ((contr.getAssociation() instanceof ManyToManyAssociation
					|| contr.getAssociation() instanceof final OneToManyAssociation otm && !otm.isBidirectional())
					&& attr.getAssociation() != null)
				findRef = true;

			if (!findRef) {
				boolean autoGenKey = false;

				if (domainAttr.isPk()
						&& domainAttr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
					autoGenKey = true;

				if (!autoGenKey) {
					b.append(targetDomainObjectName + "." + domainAttr.getSetterName() + "(" + DEFAULT_MAPPING_OBJ_NAME + getter);

					if (domainAttr.convertAttribute())
						b.append(" != null ? " + DEFAULT_MAPPING_OBJ_NAME + getter + domainAttr.getConverterExpression() + " : null");

					b.append(addTypeConversion(domainAttr));
					b.append(");\n");
				}
			}
			else {
				final AbstractDomainAssociation a = attr.getAssociation();

				if (!(a instanceof ManyToOneAssociation))
					continue;

				// A many-to-one association should be handled only once!
				if (mtoSet.contains(a))
					continue;

				final String finderFragment = createFinderFragment(attr, a.getTarget(), DEFAULT_MAPPING_OBJ_NAME + getter, true);

				if (finderFragment != null) {
					b.append(targetDomainObjectName + "." + a.getSetterName() + "(");
					b.append(finderFragment);
					b.append(");\n");

					mtoSet.add(a);
				}
				else
					createOutputForMissingRepositoryMethod(a.getTarget(), RepositoryMethodTypeEnumeration.FIND_BY_ID);
			}
		}

		final RepositoryMethod persistMethod = domainObject.findRepositoryMethod(RepositoryMethodTypeEnumeration.PERSIST);

		if (persistMethod == null)
			return createOutputForMissingRepositoryMethod(domainObject, RepositoryMethodTypeEnumeration.PERSIST);

		final Repository repository = persistMethod.getRepository();
		final String repositoryName = createRepositoryName(repository.getDomainObject());

		repositories.put(repositoryName, repository);

		b.append("\n" + targetDomainObjectName + " = " + repositoryName + "." + persistMethod.getName());
		b.append("(" + targetDomainObjectName + (persistMethod.addUniqueCheck() ? ", true" : "") + ", false, false);\n");

		if (find)
			b.append("}\n");

		b.append("\n");

		return b.toString();
	}

	/**
	 * @param contr
	 * @return the name of the association controller method
	 */
	private String createAssociationControllerMethodName(AssociationController contr) {
		final boolean create = !contr.getPersistAttributes().isEmpty();
		final boolean find = !contr.getQueryAttributes().isEmpty();
		final AbstractDomainAssociation assoc = contr.getAssociation();
		String suffix;

		if (assoc instanceof ManyToOneAssociation)
			suffix = assoc.getUpperCaseName();
		else
			suffix = assoc.getTarget().getUpperCaseName();

		if (!create)
			return CONTR_METHOD_PREFIX_FIND + suffix;

		if (find)
			return CONTR_METHOD_PREFIX_FIND_CREATE + suffix;

		return CONTR_METHOD_PREFIX_CREATE + suffix;
	}

	/**
	 * Create an additional method for the given association controller
	 * @param contr
	 * @param rootMappingObject
	 */
	protected void addAssociationControllerMethod(AssociationController contr, ExchangeMappingObject rootMappingObject) {
		final var b = new StringBuilder();
		final AbstractDomainAssociation assoc = contr.getAssociation();
		final DomainObject targetDomainObject = assoc.getTarget();
		ExchangeMappingAttribute mappingAttr = null;
		final String targetDomainObjectName = targetDomainObject.getLowerCaseName();
		boolean isOptional = false;
		boolean performQuery = true;
		final boolean create = !contr.getPersistAttributes().isEmpty();
		final boolean find = !contr.getQueryAttributes().isEmpty();
		final String methodName = createAssociationControllerMethodName(contr);
		final var methodSignature = targetDomainObject.getName() + " " + methodName + "(" + rootMappingObject.getName() + " "
				+ DEFAULT_MAPPING_OBJ_NAME + ")";
		final ExchangeMappingAttribute clientAttr = rootMappingObject.getClientAttribute();

		if (generator != null)
			generator.importPackage(targetDomainObject.getNamespace().toString());

		// Check which mode has to be implemented
		if (find)
			for (final ExchangeMappingAttribute attr : contr.getQueryAttributes()) {
				if (attr.getDomainAttribute() == null)
					continue;

				if (!attr.getDomainAttribute().isPk() && !attr.getDomainAttribute().isDisplayAttribute())
					continue;

				if (assoc instanceof ManyToOneAssociation) {
					if (attr.getAssociation() != null && attr.getAssociation().equals(assoc)) {
						mappingAttr = attr;
						performQuery = false;
						break;
					}
				}
				else if (attr.getAssociation() == null) {
					mappingAttr = attr;
					performQuery = false;
					break;
				}
			}

		if (!performQuery && !targetDomainObject.isMandated() && clientAttr == null)
			performQuery = true;

		if (assoc instanceof final ManyToOneAssociation mto)
			isOptional = mto.isOptional();

		b.append("/**\n");

		if (find)
			b.append(" * Find " + assoc.getTarget().getLabel() + " by fields of given mapping object.\n");

		if (create) {
			b.append(" * The method generates an association target object");

			if (find)
				b.append(" if respective association could not be found");

			b.append(".\n");
		}

		b.append(" * @param " + DEFAULT_MAPPING_OBJ_NAME + "\n");
		b.append(" * @return the " + assoc.getTarget().getLabel());

		if (isOptional && find && !create)
			b.append(". Null will be returned if an association object couldn't be found!");

		b.append("\n");

		if (find) {
			if (performQuery) {
				b.append(" * @throws IllegalStateException if a query returns a non-unique result");

				if (!isOptional && !create)
					b.append(" or if an association object could not be found");

				b.append("\n");
			}
			else if (!isOptional && !create)
				b.append(" * @throws IllegalStateException if association object could not be found!\n");
		}

		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		if (find) {
			b.append(targetDomainObject.getName() + " " + targetDomainObjectName + " = null;\n\n");

			if (performQuery)
				b.append(createQueryFragment(contr, targetDomainObject, rootMappingObject));
			else {
				final String getter = DEFAULT_MAPPING_OBJ_NAME + createMappingAttributeGetter(mappingAttr, rootMappingObject);
				final String finderFragment = createFinderFragment(mappingAttr, targetDomainObject, getter, true);

				if (finderFragment != null) {
					b.append(targetDomainObjectName + " = ");
					b.append(finderFragment);
					b.append(";\n\n");
				}
				else
					createOutputForMissingRepositoryMethod(targetDomainObject, RepositoryMethodTypeEnumeration.FIND_BY_ID);
			}

			if (!isOptional && !create) {
				b.append("if(" + targetDomainObject.getLowerCaseName() + " == null)\n");
				b.append("throw new IllegalStateException(\"Association \\\"");
				b.append(contr.getAssociation().getName() + "\\\" could not be initialized!\");\n\n");
			}
		}

		if (create)
			b.append(createFragmentForMissingRef(contr, targetDomainObject, rootMappingObject));

		b.append("return " + targetDomainObject.getLowerCaseName() + ";\n");
		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the handling of files after processing the import
	 * @return the generated content
	 */
	private String addFileHandlingAfterImport() {
		final var b = new StringBuilder();

		if (fileExchangeMode == null)
			return b.toString();

		final boolean deleteAfterImport = fileExchangeMode.isDeleteAfterImport();
		final boolean copyFileAfterImport = fileExchangeMode.getTargetPathAfterImport() != null
				&& !fileExchangeMode.getTargetPathAfterImport().isEmpty();
		final String sourceFileName = fileExchangeMode.getFileNamePattern();
		final String sourcePath = addDelimiterToPath(fileExchangeMode.getPath());
		var targetPath = "";
		var sourceFile = "";
		var targetFile = "";

		if (copyFileAfterImport)
			targetPath = addDelimiterToPath(fileExchangeMode.getTargetPathAfterImport());

		if (generator != null && (deleteAfterImport || copyFileAfterImport))
			generator.importPackage("java.nio.file");

		if (!method.hasPathParameter()) {
			if (sourceFileName != null && !sourceFileName.isEmpty()) {
				sourceFile = "new File(\"" + sourcePath + fileExchangeMode.getFileNamePattern() + "\")";
				targetFile = "new File(\"" + targetPath + fileExchangeMode.getFileNamePattern() + "\")";
			}
			else {
				sourceFile = "importFile";
				targetFile = "new File(\"" + targetPath + "\" + importFile.getName())";
			}
		}
		else {
			sourceFile = "new File(" + EXCHANGE_PATH_PARAM + ")";
			targetFile = "new File(\"" + targetPath + "\" + new File(pathToFile).getName())";
		}

		if (deleteAfterImport || copyFileAfterImport) {
			b.append("\n");

			if (handleMultipleFiles)
				b.append("for(final File importFile : new File(\"" + sourcePath + "\").listFiles())\n");
		}

		if (deleteAfterImport && copyFileAfterImport)
			b.append("Files.move(" + sourceFile + ".toPath(), " + targetFile + ".toPath(), StandardCopyOption.REPLACE_EXISTING);\n");
		else {
			if (copyFileAfterImport)
				b.append("Files.copy(" + sourceFile + ".toPath(), " + targetFile + ".toPath(), StandardCopyOption.REPLACE_EXISTING);\n");

			if (deleteAfterImport)
				b.append("Files.delete(" + sourceFile + ".toPath());\n");
		}

		return b.toString();
	}

	/**
	 * Add a delimiter if the path doesn't end with one!
	 * @param path
	 * @return the given path with a delimiter
	 */
	private String addDelimiterToPath(String path) {
		if (path == null || path.isEmpty())
			return path;

		if (!path.endsWith("/") && !path.endsWith("\\")) {
			if (path.contains("/"))
				path += "/";
			else
				path += "\\\\";
		}

		return path;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethod()
	 */
	@Override
	public void createMethod() {
		final var b = new StringBuilder();
		final String objectLabel = method.getRootElement().getMappingObject().getDomainObject().getLabel();
		boolean startTransaction = true;

		b.append(createMethodComment());
		b.append(addAnnotations());
		b.append("public " + getMethodSignature(true) + "\n");
		b.append("{\n");

		if (project.isJavaSEApplication())
			b.append("EntityTransaction " + TRANSACTION_OBJ_NAME + " = null;\n");
		else if (project.isSpringBootApplication()) {
			b.append("final var transactionDefinition = new DefaultTransactionDefinition();\n");
			b.append("TransactionStatus status = null;\n");
		}

		if (fileExchangeMode != null) {
			b.append("FileInputStream fileInputStream = null;\n");

			if (method.getContentType() == ContentTypeEnumeration.XML)
				b.append("InputStreamReader inputReader = null;\n");
			else if (method.getContentType() == ContentTypeEnumeration.CSV)
				b.append("Scanner scanner = null;\n");
		}

		b.append("\n");
		b.append(createInvocationDelayFragment());
		b.append("try\n");
		b.append("{\n");

		if (fileExchangeMode != null) {
			if (!method.hasPathParameter()) {
				final String path = addDelimiterToPath(fileExchangeMode.getPath());
				final String sourceFileName = fileExchangeMode.getFileNamePattern();

				if (sourceFileName != null && !sourceFileName.isEmpty()) {
					b.append("fileInputStream = new FileInputStream(new File(\"");
					b.append(path + fileExchangeMode.getFileNamePattern() + "\"));\n\n");

					if (generator != null) {
						if (method.isProcessSingleObject())
							generator.addDebugLog(b,
									"Import " + objectLabel + " from file '" + path + fileExchangeMode.getFileNamePattern() + "'");
						else
							generator.addDebugLog(b,
									"Import " + objectLabel + " objects from file '" + path + fileExchangeMode.getFileNamePattern() + "'");
					}
				}
				else {
					if (!fileExchangeMode.isNewTransactionPerFile()) {
						if (project.isJavaSEApplication()) {
							b.append(TRANSACTION_OBJ_NAME + " = em.getTransaction();\n");
							b.append(TRANSACTION_OBJ_NAME + ".begin();\n\n");
						}
						else if (project.isJakartaEEApplication())
							b.append(TRANSACTION_OBJ_NAME + ".begin();\n\n");
						else
							b.append("status = transactionManager.getTransaction(transactionDefinition);\n\n");

						startTransaction = false;
					}

					// If no file name pattern has been defined the import process should iterate over all files in a given directory!
					b.append("for(final File importFile : new File(\"" + path + "\").listFiles())\n");
					b.append("{\n");
					b.append("fileInputStream = new FileInputStream(importFile);\n\n");

					if (generator != null) {
						if (method.isProcessSingleObject())
							generator.addDebugLog(b, "Import " + objectLabel + " from file '{}'", "importFile.getAbsolutePath()");
						else
							generator.addDebugLog(b, "Import " + objectLabel + " objects from file '{}'", "importFile.getAbsolutePath()");
					}
				}
			}
			else {
				b.append("fileInputStream = new FileInputStream(new File(" + EXCHANGE_PATH_PARAM + "));\n\n");

				if (generator != null) {
					if (method.isProcessSingleObject())
						generator.addDebugLog(b, "Import " + objectLabel + " from file '{}'", EXCHANGE_PATH_PARAM);
					else
						generator.addDebugLog(b, "Import " + objectLabel + " objects from file '{}'", EXCHANGE_PATH_PARAM);
				}
			}

			b.append("\n");

			if (generator != null && method.getContentType() == ContentTypeEnumeration.XML) {
				generator.importClass("java.nio.charset.StandardCharsets");

				b.append("inputReader = new InputStreamReader(fileInputStream, " + method.getStandardCharset() + ");\n");
			}
			else if (generator != null && method.getContentType() == ContentTypeEnumeration.CSV) {
				generator.importClass("java.nio.charset.StandardCharsets");

				b.append("var fileContent = \"\";\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("scanner = new Scanner(fileInputStream, " + method.getStandardCharset());
				b.append(").useDelimiter(\"\\\\A\");\n");
				b.append("fileContent = scanner.hasNext() ? scanner.next() : \"\";\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");
				b.append("throw e;\n");
				b.append("}\n\n");
			}
		}
		else if (generator != null) {
			if (directExchangeMode) {
				if (method.isProcessSingleObject())
					generator.addDebugLog(b, "Import " + objectLabel);
				else
					generator.addDebugLog(b, "Import " + objectLabel + " objects");
			}
			else if (method.isProcessSingleObject())
				generator.addDebugLog(b, "Import " + objectLabel + " from content '{}'", method.getMethodParameters().get(0).getName());
			else
				generator.addDebugLog(b, "Import " + objectLabel + " objects from content '{}'",
						method.getMethodParameters().get(0).getName());

			b.append("\n");
		}

		if (!directExchangeMode)
			b.append(addUnmarshallerInvocation());

		// Normally, a stream should be closed in the respective finally block! But we must also cover cases where multiple files are
		// imported!
		if (fileExchangeMode != null && !method.hasPathParameter()
				&& (fileExchangeMode.getFileNamePattern() == null || fileExchangeMode.getFileNamePattern().isEmpty())) {
			if (method.getContentType() == ContentTypeEnumeration.XML)
				b.append("inputReader.close();\n\n");
			else if (method.getContentType() == ContentTypeEnumeration.CSV)
				b.append("scanner.close();\n\n");
			else
				b.append("fileInputStream.close();\n\n");
		}

		if (startTransaction) {
			if (project.isJavaSEApplication()) {
				b.append(TRANSACTION_OBJ_NAME + " = em.getTransaction();\n");
				b.append(TRANSACTION_OBJ_NAME + ".begin();\n\n");
			}
			else if (project.isJakartaEEApplication())
				b.append(TRANSACTION_OBJ_NAME + ".begin();\n\n");
			else
				b.append("status = transactionManager.getTransaction(transactionDefinition);\n\n");
		}

		if (method.isProcessSingleObject())
			b.append(createMethodForSingleObject());
		else
			b.append(createMethodForMultipleObjects());

		if (handleMultipleFiles) {
			if (fileExchangeMode.isNewTransactionPerFile()) {
				if (!project.isSpringBootApplication())
					b.append("\n" + TRANSACTION_OBJ_NAME + ".commit();\n");
				else
					b.append("\ntransactionManager.commit(status);\n");
			}

			b.append("}\n");

			if (!fileExchangeMode.isNewTransactionPerFile()) {
				if (!project.isSpringBootApplication())
					b.append("\n" + TRANSACTION_OBJ_NAME + ".commit();\n");
				else
					b.append("\ntransactionManager.commit(status);\n");
			}
		}
		else if (!project.isSpringBootApplication())
			b.append("\n" + TRANSACTION_OBJ_NAME + ".commit();\n");
		else
			b.append("\ntransactionManager.commit(status);\n");

		b.append(addFileHandlingAfterImport());

		if (generator != null) {
			b.append("\n");

			generator.addDebugLog(b, "Data import operation finished");
		}

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		if (project.isJavaSEApplication()) {
			b.append("if(" + TRANSACTION_OBJ_NAME + " != null && " + TRANSACTION_OBJ_NAME + ".isActive())\n");
			b.append(TRANSACTION_OBJ_NAME + ".rollback();\n\n");
		}
		else if (project.isJakartaEEApplication()) {
			b.append("try\n");
			b.append("{\n");
			b.append("if(" + TRANSACTION_OBJ_NAME + ".getStatus() != Status.STATUS_NO_TRANSACTION)\n");
			b.append(TRANSACTION_OBJ_NAME + ".rollback();\n");
			b.append("}\n");
			b.append("catch (final Exception te)\n");
			b.append("{\n");

			if (generator != null)
				generator.addErrorLog(b, "Error while performing transaction rollback!", "te");

			b.append("}\n\n");
		}
		else {
			b.append("if(status != null)\n");
			b.append("transactionManager.rollback(status);\n\n");
		}

		b.append("throw new DataImportException(e, true);\n");
		b.append("}\n");

		if (fileExchangeMode != null) {
			b.append("finally\n");
			b.append("{\n");

			if (method.getContentType() == ContentTypeEnumeration.XML)
				b.append("if(inputReader != null)\n");
			else if (method.getContentType() == ContentTypeEnumeration.CSV)
				b.append("if(scanner != null)\n");
			else
				b.append("if(fileInputStream != null)\n");

			b.append("try\n");
			b.append("{\n");

			if (method.getContentType() == ContentTypeEnumeration.XML)
				b.append("inputReader.close();\n");
			else if (method.getContentType() == ContentTypeEnumeration.CSV)
				b.append("scanner.close();\n");
			else
				b.append("fileInputStream.close();\n");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			if (generator != null)
				generator.addWarningLog(b, "Could not close data import resource!", "e");

			b.append("}\n");
			b.append("}\n");
		}

		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(getMethodSignature(true), b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodForSingleObject()
	 */
	@Override
	protected String createMethodForSingleObject() {
		final var b = new StringBuilder();
		final ExchangeMappingObject rootMappingObject = method.getRootElement().getMappingObject();

		b.append(createImportForElement(method.getRootElement(), null, rootMappingObject.isAddNewItems(), false,
				rootMappingObject.isUpdateExistingItems(), rootMappingObject.isDeleteAllItems()));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#createMethodForMultipleObjects()
	 */
	@Override
	protected String createMethodForMultipleObjects() {
		final var b = new StringBuilder();
		final ExchangeMappingObject rootMappingObject = method.getRootElement().getMappingObject();

		if (rootMappingObject.isDeleteAllItems()) {
			final RepositoryMethod deleteMethod = rootMappingObject.getDomainObject()
					.findRepositoryMethod(RepositoryMethodTypeEnumeration.DELETE_ALL);

			if (deleteMethod != null) {
				b.append("// Delete all existing " + rootMappingObject.getDomainObject().getLabelPlural() + "!\n");
				b.append(createRepositoryName(deleteMethod.getRepository().getDomainObject()) + "." + deleteMethod.getName() + "();\n\n");
			}
		}

		if (method.getContentType() == ContentTypeEnumeration.XML) {
			method.getRootElement().getSubElements().forEach(element -> {
				b.append("for(final " + element.getMappingObject().getName() + " " + DEFAULT_MAPPING_OBJ_NAME);
				b.append(" : " + DEFAULT_ROOT_MAPPING_OBJ_NAME + "." + element.getMappingAttribute().getGetterName() + ")\n");
				b.append("{\n");
				b.append(createImportForElement(element, null, rootMappingObject.isAddNewItems(), rootMappingObject.isDeleteAllItems(),
						rootMappingObject.isUpdateExistingItems(), false));
				b.append("}\n");
			});
		}
		else {
			b.append("for(final " + method.getRootElement().getMappingObject().getName());
			b.append(" " + DEFAULT_MAPPING_OBJ_NAME + " : " + DATA_IMPORT_LIST + ")\n");
			b.append("{\n");
			b.append(createImportForElement(method.getRootElement(), null, rootMappingObject.isAddNewItems(),
					rootMappingObject.isDeleteAllItems(), rootMappingObject.isUpdateExistingItems(), false));
			b.append("}\n");
		}

		return b.toString();
	}

	/**
	 * @param attr
	 * @param targetObjectName
	 * @param sourceObjectName
	 * @param parentMappingGetter
	 * @param updateMode
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String createAttributeImportMapping(ExchangeMappingAttribute attr, String targetObjectName, String sourceObjectName,
			String parentMappingGetter, boolean updateMode, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();
		final ExchangeMappingObject mappingObject = attr.getExchangeMappingObject();

		if (attr.getDomainAttribute() == null)
			return b.toString();

		// We don't convert fields that are controlled by the persistence provider!
		if (attr.getDomainAttribute().isSetDateOnPersist() || attr.getDomainAttribute().isSetDateOnUpdate())
			return b.toString();

		// In case of insert operations it makes no sense to fill a version control field!
		if (!updateMode && attr.getDomainAttribute().isTrackVersion())
			return b.toString();

		if (attr.getAssociation() == null) {
			if (mappingObject.getPKAttribute() != null && mappingObject.getPKAttribute().equals(attr) && (updateMode
					|| attr.getDomainAttribute().getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE))
				return b.toString();
		}
		else {
			if (attr.getAssociation() instanceof final ManyToOneAssociation mto) {
				if (updateMode && !mto.isUpdatable())
					return b.toString();

				if (!updateMode && !mto.isInsertable())
					return b.toString();

				final boolean drivenByController = getAssociationController(mto) != null;

				if (drivenByController) {
					// The association is controlled by a special method. This method should be invoked only once per insert or update
					// operation!
					if (updateMode) {
						if (controlledAssocSetUpdate.contains(mto))
							return b.toString();

						controlledAssocSetUpdate.add(mto);
					}
					else {
						if (controlledAssocSetNew.contains(mto))
							return b.toString();

						controlledAssocSetNew.add(mto);
					}
				}
				else {
					final boolean drivenByPrimaryKey = isAssociationDrivenByPrimaryKey(mto);

					if (drivenByPrimaryKey) {
						if (!mto.getTarget().getPKAttribute().equals(attr.getDomainAttribute()))
							return b.toString();
					}
					else {
						final DomainAttribute displayAttr = mto.getTarget().getDisplayAttribute();

						if (displayAttr == null || !displayAttr.equals(attr.getDomainAttribute()))
							return b.toString();
					}
				}
			}

			// If an attribute represents a primary key of an object that is referenced by a one-to-one association we can also skip it!
			if (attr.getAssociation() instanceof OneToOneAssociation
					&& attr.getAssociation().getTarget().getPKAttribute().equals(attr.getDomainAttribute())
					&& (updateMode || attr.getAssociation().getTarget().getPKAttribute().getDomainObject().getIDGenerator()
							.getGeneratorType() != IDGeneratorTypeEnumeration.NONE))
				return b.toString();
		}

		if (attr.getAssociation() == null || attr.getAssociation() instanceof OneToOneAssociation) {
			if (updateMode) {
				// We skip fields that shouldn't be changed!
				if (!attr.isUpdatable() || !attr.getDomainAttribute().isUpdatable())
					return b.toString();
			}
			else if (!attr.isInsertable() || !attr.getDomainAttribute().isInsertable()) {
				// We skip fields that are not intended to be used in insert operations!
				return b.toString();
			}
		}

		var getter = "";

		if (parentMappingGetter != null && !parentMappingGetter.isEmpty())
			getter = parentMappingGetter + ".";

		getter += attr.getGetterName();

		if (attr.getAssociation() != null) {
			final AbstractDomainAssociation assoc = attr.getAssociation();

			if (assoc instanceof ManyToOneAssociation)
				b.append(createMappingForManyToOneAssoc(attr, updateMode, targetObjectName, getter, parentAssoc));
			else if (assoc instanceof OneToOneAssociation)
				b.append(createMappingForOneToOneAssoc(attr, updateMode, targetObjectName, getter, parentAssoc));
		}
		else {
			b.append(targetObjectName + "." + attr.getDomainAttribute().getSetterName() + "(" + sourceObjectName + "." + getter);

			if (attr.getDomainAttribute().convertAttribute()) {
				b.append(" != null ? " + sourceObjectName + "." + getter);
				b.append(attr.getDomainAttribute().getConverterExpression() + " : null");
			}

			b.append(addTypeConversion(attr.getDomainAttribute()));
			b.append(");\n");
		}

		return b.toString();
	}

	/**
	 * @param attr
	 * @param updateMode
	 * @param targetDomainObjectName
	 * @param parentGetter
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String createMappingForOneToOneAssoc(ExchangeMappingAttribute attr, boolean updateMode, String targetDomainObjectName,
			String parentGetter, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();
		final var setter = new StringBuilder();
		final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(attr, parentAssoc);

		setter.append(targetDomainObjectName + ".");

		// It might be the case that an attribute of a one-to-one association is not directly accessible via the selected domain
		// object!
		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);
			setter.append(ia.getGetterName() + ".");
		}

		setter.append(attr.getDomainAttribute().getSetterName());

		// If the attribute is part of a complex hierarchy it must be checked if it can be accessed!
		final String ifStatement = createDomainAssocIfStatement(attr, parentAssoc, targetDomainObjectName, updateMode);

		if (!ifStatement.isEmpty()) {
			b.append("\n");
			b.append(ifStatement);
		}

		b.append(setter.toString() + "(" + DEFAULT_MAPPING_OBJ_NAME + "." + parentGetter);

		if (attr.getDomainAttribute().convertAttribute()) {
			b.append(" != null ? " + DEFAULT_MAPPING_OBJ_NAME + "." + parentGetter);
			b.append(attr.getDomainAttribute().getConverterExpression() + " : null");
		}

		b.append(addTypeConversion(attr.getDomainAttribute()));
		b.append(");\n");

		return b.toString();
	}

	/**
	 * @param attr
	 * @param updateMode
	 * @param targetDomainObjectName
	 * @param parentGetter
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String createMappingForManyToOneAssoc(ExchangeMappingAttribute attr, boolean updateMode,
			String targetDomainObjectName, String parentGetter, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();
		final var mto = (ManyToOneAssociation) attr.getAssociation();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();
		final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(attr, parentAssoc);
		final var ifFragment = new StringBuilder();
		final var setter = new StringBuilder();
		final var s = new StringBuilder();
		boolean firstIf = true;
		boolean refNotSet = false;

		setter.append(targetDomainObjectName + ".");

		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);

			boolean optional = false;

			if (ia instanceof ManyToOneAssociation) {
				// We don't want to change many-to-one associations of deeper levels!
				return b.toString();
			}

			if (ia instanceof final OneToOneAssociation oto)
				optional = oto.isOptional();

			s.append(ia.getGetterName());

			// Only in case of update operations we must check if the one-to-one association really exists!
			if (optional && updateMode) {
				if (firstIf) {
					firstIf = false;
					ifFragment.append("\n");
				}

				ifFragment.append("if(" + targetDomainObjectName + "." + s.toString() + " != null)\n");
			}

			s.append(".");
		}

		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);
			setter.append(ia.getGetterName() + ".");
		}

		setter.append(mto.getSetterName());

		b.append(ifFragment);

		if (mto.isOptional()) {
			final JavaType type = domainAttribute.getJavaType();

			if (firstIf)
				b.append("\n");

			b.append("if(" + DEFAULT_MAPPING_OBJ_NAME + "." + parentGetter + " != ");
			b.append(attr.getJavaType().getLocalVariableDefaultValue());

			if (type.isString())
				b.append(" && !" + DEFAULT_MAPPING_OBJ_NAME + "." + parentGetter + ".isEmpty()");

			b.append(")\n");
		}

		b.append(setter.toString() + "(");

		if (getAssociationController(mto) != null) {
			final String controllerMethodName = createAssociationControllerMethodName(getAssociationController(mto));
			var controlGetter = "";

			// The parameter 'parentGetter' contains the full path to access a given field. But for invoking the controller method we
			// must cut off the field getter!
			if (parentGetter != null && !parentGetter.isEmpty() && parentGetter.lastIndexOf('.') > 0)
				controlGetter = parentGetter.substring(0, parentGetter.lastIndexOf('.'));

			if (!controlGetter.isEmpty())
				controlGetter = "." + controlGetter;

			b.append(controllerMethodName + "(" + DEFAULT_MAPPING_OBJ_NAME + controlGetter + ")");
		}
		else {
			final var finderFragment = createFinderFragment(attr, mto.getTarget(), DEFAULT_MAPPING_OBJ_NAME + "." + parentGetter, true);

			if (finderFragment == null) {
				refNotSet = true;
				b.append("null");
			}
			else
				b.append(finderFragment);
		}

		b.append(");");

		if (refNotSet)
			b.append(" // TODO: Reference field must be set manually as it couldn't be determined by generator!");

		b.append("\n");

		if (mto.isOptional()) {
			b.append("else\n");
			b.append(setter.toString() + "(null);\n\n");
		}

		return b.toString();
	}

	/**
	 * Create the mapping for many-to-many and unidirectional one-to-many associations
	 * @param subElement
	 * @param sourceMappingObjectName
	 * @param targetObjectName
	 * @param parentMappingGetter
	 * @param updateMode
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String createMappingForAssociationList(DataExchangeElement subElement, String sourceMappingObjectName,
			String targetObjectName, String parentMappingGetter, boolean updateMode, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();
		final ExchangeMappingObject mappingObject = subElement.getMappingObject();
		final ExchangeMappingAttribute mappingAttr = subElement.getMappingAttribute();
		final AbstractDomainAssociation assoc = subElement.getMappingAttribute().getAssociation();
		final AssociationController controller = getAssociationController(assoc);
		final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(mappingAttr, parentAssoc);
		final var getter = new StringBuilder();
		ExchangeMappingAttribute finderAttribute = mappingObject.getPKAttribute();
		boolean refNotSet = false;

		if (controller == null) {
			if (finderAttribute == null)
				finderAttribute = mappingObject.getDisplayAttribute();

			if (finderAttribute == null)
				return "";
		}

		// It doesn't make sense to generate code if this association doesn't represent the owner!
		if (!assoc.isOwner())
			return "";

		b.append("\n");

		// It might be the case that an association is not directly accessible via the selected domain object!
		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);
			getter.append("." + ia.getGetterName());
		}

		getter.append("." + assoc.getGetterName());

		// We should only access the association if all parent associations are set properly!
		final String ifStatement = createDomainAssocIfStatement(mappingAttr, parentAssoc, targetObjectName, updateMode);

		b.append(ifStatement);

		if (!ifStatement.isEmpty())
			b.append("{\n");

		if (updateMode)
			b.append(targetObjectName + getter.toString() + ".clear();\n\n");

		b.append("for(final " + mappingAttr.getJavaType().getName() + " " + DEFAULT_LIST_ITEM_NAME);
		b.append(" : " + sourceMappingObjectName + "." + parentMappingGetter + ")\n");
		b.append(targetObjectName + getter.toString() + ".add(");

		if (controller != null) {
			final String controllerMethodName = createAssociationControllerMethodName(controller);

			b.append(controllerMethodName + "(" + DEFAULT_LIST_ITEM_NAME + ")");
		}
		else {
			final var getterForFinder = DEFAULT_LIST_ITEM_NAME + "." + finderAttribute.getGetterName();
			final String finderFragment = createFinderFragment(finderAttribute, assoc.getTarget(), getterForFinder, true);

			if (finderFragment == null) {
				refNotSet = true;
				b.append("null");
			}
			else
				b.append(finderFragment);
		}

		b.append(");");

		if (refNotSet)
			b.append(" // TODO: Reference field must be set manually as it couldn't be determined by generator!");

		b.append("\n");

		if (!ifStatement.isEmpty())
			b.append("}\n");

		return b.toString();
	}

	/**
	 * @param element
	 */
	protected void addOneToManyMethods(DataExchangeElement element) {
		final var b = new StringBuilder();
		final ExchangeMappingAttribute attr = element.getMappingAttribute();
		final ExchangeMappingObject mappingObject = element.getMappingObject();

		if (mappingObject == null || !element.isContainer() || attr == null)
			return;

		final AbstractDomainAssociation assoc = attr.getAssociation();

		if (!(assoc instanceof final OneToManyAssociation otm) || !otm.isBidirectional())
			return;

		final var methodName = "import" + assoc.getUpperCaseName();
		var methodSignature = "void " + methodName + "(" + assoc.getDomainObject().getName() + " "
				+ assoc.getDomainObject().getLowerCaseName();
		methodSignature += ", " + element.getParentElement().getMappingObject().getName() + " " + DEFAULT_ROOT_MAPPING_OBJ_NAME + ")";

		if (methodAssocSet.contains(methodSignature))
			return;

		final DomainObject domainObject = mappingObject.getDomainObject();

		b.append("/**\n");
		b.append(" * Import " + domainObject.getLabel() + " objects\n");
		b.append(" * @param " + assoc.getDomainObject().getLowerCaseName() + "\n");
		b.append(" * @param " + DEFAULT_ROOT_MAPPING_OBJ_NAME + "\n");
		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		if (attr.isDeleteAllItems()) {
			final String repositoryName = createRepositoryName(domainObject);

			b.append("// Delete all existing " + domainObject.getLabelPlural() + " that belong to this ");
			b.append(assoc.getDomainObject().getLabel() + "\n");
			b.append(repositoryName + "." + REPO_METHOD_NAME_DELETE + "(" + assoc.getDomainObject().getLowerCaseName() + ".");
			b.append(attr.getGetterName() + ");\n");
			b.append(assoc.getDomainObject().getLowerCaseName() + "." + attr.getGetterName() + ".clear();\n\n");
		}

		b.append("for(final " + mappingObject.getName() + " " + DEFAULT_MAPPING_OBJ_NAME);
		b.append(" : " + DEFAULT_ROOT_MAPPING_OBJ_NAME + "." + attr.getGetterName() + ")\n");
		b.append("{\n");
		b.append(createImportForElement(element, assoc, attr.isAddNewItems(), attr.isDeleteAllItems(), attr.isUpdateExistingItems(),
				false));
		b.append("}\n");
		b.append("}\n\n");

		methodAssocSet.add(methodSignature);

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * @param element
	 */
	protected void addOneToOneMethods(DataExchangeElement element) {
		final var b = new StringBuilder();
		final ExchangeMappingAttribute attr = element.getMappingAttribute();
		final ExchangeMappingObject mappingObject = element.getMappingObject();

		if (mappingObject == null || !element.isContainer() || attr == null)
			return;

		final String targetObjectName = mappingObject.getDomainObject().getLowerCaseName();
		final AbstractDomainAssociation assoc = attr.getAssociation();

		if (!(assoc instanceof final OneToOneAssociation oto))
			return;

		final var methodName = "import" + assoc.getUpperCaseName();
		var methodSignature = "void " + methodName + "(" + assoc.getDomainObject().getName() + " "
				+ assoc.getDomainObject().getLowerCaseName();
		methodSignature += ", " + mappingObject.getName() + " " + DEFAULT_MAPPING_OBJ_NAME + ")";

		if (methodAssocSet.contains(methodSignature))
			return;

		final DomainObject domainObject = mappingObject.getDomainObject();

		b.append("/**\n");
		b.append(" * Import " + domainObject.getLabel() + "\n");
		b.append(" * @param " + assoc.getDomainObject().getLowerCaseName() + "\n");
		b.append(" * @param " + DEFAULT_MAPPING_OBJ_NAME + "\n");
		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		final RepositoryMethod persistMethod = mappingObject.getDomainObject()
				.findRepositoryMethod(RepositoryMethodTypeEnumeration.PERSIST);

		if (persistMethod != null) {
			final Repository repository = persistMethod.getRepository();
			final String repoName = createRepositoryName(repository.getDomainObject());

			// In case of one-to-one associations we don't have to search for the referenced object! We assume that we don't have to
			// change a given reference. Rather we only change fields of the referenced object!
			b.append(mappingObject.getDomainObject().getName() + " " + targetObjectName + " = ");
			b.append(assoc.getDomainObject().getLowerCaseName() + "." + assoc.getGetterName() + ";\n\n");
			b.append("if(" + targetObjectName + " != null)\n");
			b.append("{\n");
			b.append(importDataExchangeElement(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName, repoName, null, true, assoc));
			b.append("}\n");
			b.append("else\n");
			b.append("{\n");

			if (generator != null)
				generator.importPackage(mappingObject.getDomainObject().getNamespace().toString());

			b.append(targetObjectName + " = new " + mappingObject.getDomainObject().getName() + "();\n");
			b.append(initializeOneToOneAssociations(targetObjectName, mappingObject, assoc, assoc));

			// In case of bidirectional associations we must wire both objects!
			final OneToOneAssociation reverseAssoc = oto.getReverseAssociation();

			if (reverseAssoc != null) {
				b.append(targetObjectName + "." + reverseAssoc.getSetterName() + "(");
				b.append(assoc.getDomainObject().getLowerCaseName() + ");\n");
			}

			b.append(importDataExchangeElement(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName, repoName, null, false, assoc));
			b.append(addOneToOnePersistOperations(element, targetObjectName, assoc, assoc));

			// If the respective cascade type is set we won't call the persist method!
			if (oto.isCascadePersist()) {
				repositories.put(repoName, repository);

				b.append("\n" + targetObjectName + " = " + repoName + "." + persistMethod.getName());
				b.append("(" + targetObjectName + (persistMethod.addUniqueCheck() ? ", true" : "") + ", false, false);\n");
			}

			b.append(assoc.getDomainObject().getLowerCaseName() + "." + assoc.getSetterName() + "(" + assoc.getName() + ");\n");

			// One-to-many associations must be handled after performing the persist operation of the parent object
			b.append(addOneToManyInvocations(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName));
			b.append("}\n");
		}
		else
			b.append(createOutputForMissingRepositoryMethod(mappingObject.getDomainObject(), RepositoryMethodTypeEnumeration.PERSIST));

		b.append("}\n\n");

		methodAssocSet.add(methodSignature);

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * The number of associations necessary to iterate through in order to generate a getter/setter fragment must be determined
	 * dynamically!
	 * @param attr
	 * @param parentAssoc
	 * @return all associations in a list
	 */
	public EList<AbstractDomainAssociation> getDomainAttributeAssocs(ExchangeMappingAttribute attr,
			AbstractDomainAssociation parentAssoc) {
		final var assocList = new BasicEList<AbstractDomainAssociation>();

		if (attr.getAssociation() == null)
			return assocList;

		if (attr.getAssociation().equals(parentAssoc))
			return assocList;

		if (attr.getAssociation() instanceof OneToOneAssociation)
			assocList.add(attr.getAssociation());

		if (!attr.getAssociationList().isEmpty())
			for (final AbstractDomainAssociation ia : attr.getAssociationList()) {
				if (ia.equals(parentAssoc))
					break;

				assocList.add(ia);
			}

		return assocList;
	}

	/**
	 * @param element
	 * @return a list containing all attributes of a given element that should be used to search for a root domain object
	 */
	private EList<ExchangeMappingAttribute> getFinderAttributes(DataExchangeElement element) {
		final var list = new BasicEList<ExchangeMappingAttribute>();

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			final ExchangeMappingAttribute mappingAttr = attr.getMappingAttribute();

			if (mappingAttr == null || mappingAttr.getDomainAttribute() == null || !attr.isUsedForCustomQuery())
				continue;

			list.add(mappingAttr);
		}

		for (final DataExchangeElement e : element.getSubElements()) {
			final ExchangeMappingAttribute mappingAttr = e.getMappingAttribute();
			AbstractDomainAssociation mappingAssoc = null;

			if (mappingAttr != null) {
				// Many-to-many and one-to-many container elements must not be considered!
				if (e.isContainer()) {
					mappingAssoc = mappingAttr.getAssociation();

					if (mappingAssoc != null
							&& (mappingAssoc instanceof OneToManyAssociation || mappingAssoc instanceof ManyToManyAssociation))
						continue;
				}

				if (mappingAttr.getDomainAttribute() != null && e.isUsedForCustomQuery())
					list.add(mappingAttr);
			}

			// Search for further attributes in all sub-elements
			list.addAll(getFinderAttributes(e));
		}

		return list;
	}

	/**
	 * @return a list containing all attributes that should be used to search a root domain object
	 */
	private EList<ExchangeMappingAttribute> getFinderAttributes() {
		EList<ExchangeMappingAttribute> list;
		DataExchangeElement rootElement = method.getRootElement();

		if (!method.isProcessSingleObject() && method.getContentType() == ContentTypeEnumeration.XML)
			rootElement = method.getRootElement().getSubElements().get(0);

		list = getFinderAttributes(rootElement);

		return list;
	}

	/**
	 * @return true if a root domain object should be searched by respective attributes
	 */
	private boolean usesCustomFinderMethod() {
		return !getFinderAttributes().isEmpty();
	}

	/**
	 * Add a custom finder method
	 */
	protected void addCustomFinderMethod() {
		final var b = new StringBuilder();
		DataExchangeElement rootElement = method.getRootElement();

		if (!method.isProcessSingleObject() && method.getContentType() == ContentTypeEnumeration.XML)
			rootElement = rootElement.getSubElements().get(0);

		final DomainObject domainObject = rootElement.getMappingObject().getDomainObject();
		final String domainObjectName = domainObject.getName();
		final String targetObjName = domainObject.getLowerCaseName();
		final String methodName = FINDER_METHOD_PREFIX + domainObjectName;
		final var methodSignature = domainObjectName + " " + methodName + "(" + rootElement.getMappingObject().getName() + " "
				+ DEFAULT_MAPPING_OBJ_NAME + ")";

		if (generator != null)
			generator.importPackage(PACK_JAVA_UTIL);

		b.append("/**\n");
		b.append(" * Find " + domainObject.getLabel() + " by fields of given mapping object.\n");
		b.append(" * @param " + DEFAULT_MAPPING_OBJ_NAME + "\n");
		b.append(" * @return the persistent " + domainObject.getLabel() + " object. ");
		b.append("Null will be returned if it couldn't be found!\n");
		b.append(" * @throws IllegalStateException if a query returns a non-unique result\n");
		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append(domainObjectName + " " + targetObjName + " = null;\n\n");

		// Create the query statement
		b.append("final var queryStatement = \"select a from " + domainObjectName + " a where ");

		boolean firstAttr = true;
		int paramCount = 1;

		for (final ExchangeMappingAttribute attr : getFinderAttributes()) {
			final AbstractDomainAssociation assoc = attr.getAssociation();

			if (firstAttr)
				firstAttr = false;
			else
				b.append(" and ");

			b.append("a.");

			if (assoc != null) {
				attr.getAssociationList().forEach(a -> b.append(a.getName() + "."));

				b.append(assoc.getName() + ".");
			}

			b.append(attr.getDomainAttribute().getName() + " = :param" + paramCount++);
		}

		b.append("\";\n");
		b.append("final var parameterMap = new HashMap<String, Object>();\n\n");

		paramCount = 1;

		// Set all query parameters
		for (final ExchangeMappingAttribute attr : getFinderAttributes()) {
			final String mappingGetter = createMappingAttributeGetter(attr, rootElement.getMappingObject());

			b.append("parameterMap.put(\"param" + paramCount++ + "\", " + DEFAULT_MAPPING_OBJ_NAME + mappingGetter + ");\n");
		}

		b.append("\n");

		final RepositoryMethod searchMethod = domainObject.findRepositoryMethod(RepositoryMethodTypeEnumeration.SEARCH);

		if (searchMethod == null) {
			b.append(createOutputForMissingRepositoryMethod(domainObject, RepositoryMethodTypeEnumeration.SEARCH));
			b.append("return null;\n}\n");
		}
		else {
			final Repository repository = searchMethod.getRepository();
			final String repositoryName = createRepositoryName(repository.getDomainObject());

			b.append("// We actually search for only one object but we want to know if the query really returns a unique result!\n");
			b.append("final List<" + domainObjectName + "> " + DEFAULT_OBJECT_LIST_NAME + " = ");
			b.append(repositoryName + "." + searchMethod.getName() + "(queryStatement, 2, 0, parameterMap);\n\n");
			b.append("if(" + DEFAULT_OBJECT_LIST_NAME + ".size() == 1)\n");
			b.append(targetObjName + " = " + DEFAULT_OBJECT_LIST_NAME + ".get(0);\n\n");
			b.append("if(" + DEFAULT_OBJECT_LIST_NAME + ".size() > 1)\n");
			b.append("throw new IllegalStateException(\"Query to find persistent domain object \\\"");
			b.append(domainObjectName + "\\\" returned non-unique result!\");\n\n");
			b.append("return " + targetObjName + ";\n");
			b.append("}\n\n");
		}

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * @param targetObjectName
	 * @param mappingObject
	 * @param assocToBeExcluded
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String initializeOneToOneAssociations(String targetObjectName, ExchangeMappingObject mappingObject,
			AbstractDomainAssociation assocToBeExcluded, AbstractDomainAssociation parentAssoc) {
		// Initialize all one-to-one associations!
		final var oneToOneAttrMap = new HashMap<ExchangeMappingAttribute, Integer>();
		final var assocSet = new HashSet<AbstractDomainAssociation>();
		final var b = new StringBuilder();

		for (final ExchangeMappingAttribute mappingAttr : mappingObject.getAttributes()) {
			// If the attribute is not mapped to a domain attribute the association will be handled in a separate method!
			if (mappingAttr.getDomainAttribute() == null)
				continue;

			final AbstractDomainAssociation mappingAssoc = mappingAttr.getAssociation();

			if (mappingAssoc == null)
				continue;

			// If the calling method handles the (one-to-one) association it shouldn't be initialized at this point!
			if (mappingAssoc.equals(assocToBeExcluded))
				continue;

			if (mappingAssoc instanceof OneToOneAssociation && !assocSet.contains(mappingAssoc)) {
				oneToOneAttrMap.put(mappingAttr, mappingAttr.getAssociationList().size());
				assocSet.add(mappingAssoc);
			}
		}

		final List<Map.Entry<ExchangeMappingAttribute, Integer>> list = new LinkedList<>(oneToOneAttrMap.entrySet());

		// We must sort the mapping attributes in order to persist respective objects in deeper levels first!
		list.sort((obj1, obj2) -> obj1.getValue().compareTo(obj2.getValue()));

		list.stream().map(Entry::getKey).forEach(mappingAttr -> {
			final AbstractDomainAssociation mappingAssoc = mappingAttr.getAssociation();

			if (generator != null)
				generator.importPackage(mappingAssoc.getTarget().getNamespace().toString());

			b.append("\nvar " + mappingAssoc.getName() + " = new " + mappingAssoc.getTarget().getName() + "();\n");
			b.append(targetObjectName + "." + getOneToOneGetter(mappingAttr, parentAssoc) + mappingAssoc.getSetterName());
			b.append("(" + mappingAssoc.getName() + ");\n");
		});

		if (!list.isEmpty())
			b.append("\n");

		return b.toString();
	}

	/**
	 * @param mappingAttr
	 * @param parentAssoc
	 * @return the generated multi-level getter method for cascaded one-to-one associations. The method returns an empty string if
	 *         the association list is empty or if it contains associations other than one-to-one!
	 */
	protected String getOneToOneGetter(ExchangeMappingAttribute mappingAttr, AbstractDomainAssociation parentAssoc) {
		final var assocGetter = new StringBuilder();
		final AbstractDomainAssociation mappingAssoc = mappingAttr.getAssociation();
		final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(mappingAttr, parentAssoc);

		for (int i = assocList.size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation ia = assocList.get(i);

			if (ia instanceof OneToOneAssociation) {
				if (!ia.equals(mappingAssoc)) {
					assocGetter.append(ia.getGetterName());
					assocGetter.append(".");
				}

				continue;
			}

			// We only go through a list of one-to-one associations!
			return "";
		}

		return assocGetter.toString();
	}

	/**
	 * @param element
	 * @param sourceMappingObjectName
	 * @param targetObjectName
	 * @return the generated content
	 */
	protected String addOneToManyInvocations(DataExchangeElement element, String sourceMappingObjectName, String targetObjectName) {
		final var b = new StringBuilder();

		if (element.getMappingObject() == null || !element.isContainer())
			return b.toString();

		for (final DataExchangeElement subElement : element.getSubElements()) {
			final ExchangeMappingAttribute attr = subElement.getMappingAttribute();

			if (!subElement.isContainer() || attr == null || attr.getAssociation() == null)
				continue;

			final AbstractDomainAssociation assoc = subElement.getMappingAttribute().getAssociation();

			if (assoc instanceof final OneToManyAssociation otm && otm.isBidirectional()) {
				// We must call the respective method in order to import the data
				final var methodName = "import" + assoc.getUpperCaseName();

				b.append("\n// Import " + assoc.getTarget().getLabel() + " objects\n");
				b.append(methodName + "(" + targetObjectName + ", " + sourceMappingObjectName + ");\n");
			}
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @param sourceMappingObjectName
	 * @param targetObjectName
	 * @param repositoryName
	 * @param parentMappingGetter
	 * @param updateMode
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String importDataExchangeElement(DataExchangeElement element, String sourceMappingObjectName, String targetObjectName,
			String repositoryName, String parentMappingGetter, boolean updateMode, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();

		if (element.getMappingObject() == null || !element.isContainer())
			return b.toString();

		for (final DataExchangeElement subElement : element.getSubElements()) {
			if (subElement.isContainer())
				continue;

			if (subElement.getMappingAttribute() == null)
				continue;

			b.append(createAttributeImportMapping(subElement.getMappingAttribute(), targetObjectName, sourceMappingObjectName,
					parentMappingGetter, updateMode, parentAssoc));
		}

		for (final DataExchangeAttribute attribute : element.getAttributes()) {
			if (attribute.getMappingAttribute() == null)
				continue;

			b.append(createAttributeImportMapping(attribute.getMappingAttribute(), targetObjectName, sourceMappingObjectName,
					parentMappingGetter, updateMode, parentAssoc));
		}

		for (final DataExchangeElement subElement : element.getSubElements()) {
			final ExchangeMappingAttribute mappingAttr = subElement.getMappingAttribute();

			if (!subElement.isContainer() || mappingAttr == null || mappingAttr.getAssociation() == null)
				continue;

			final AbstractDomainAssociation assoc = mappingAttr.getAssociation();
			var mappingGetter = "";

			if (parentMappingGetter != null && !parentMappingGetter.isEmpty())
				mappingGetter = parentMappingGetter + ".";

			if (assoc instanceof ManyToManyAssociation || assoc instanceof final OneToManyAssociation otm && !otm.isBidirectional()) {
				mappingGetter += mappingAttr.getGetterName();
				b.append(createMappingForAssociationList(subElement, sourceMappingObjectName, targetObjectName, mappingGetter, updateMode,
						parentAssoc));
			}
			else if (assoc instanceof ManyToOneAssociation) {
				mappingGetter += mappingAttr.getGetterName();
				b.append(importDataExchangeElement(subElement, sourceMappingObjectName, targetObjectName, repositoryName, mappingGetter,
						updateMode, parentAssoc));
			}
			else if (assoc instanceof final OneToOneAssociation oto) {
				// We must call the respective method in order to import the data
				final var methodName = "import" + assoc.getUpperCaseName();

				b.append("\n// Import " + assoc.getTarget().getLabel() + "\n");

				// If the data source doesn't provide a respective mapping object we'll have to skip the import!
				if (oto.isOptional() || subElement.getMinOccurrences() == 0)
					b.append("if(" + sourceMappingObjectName + "." + mappingAttr.getGetterName() + " != null)\n");

				b.append(
						methodName + "(" + targetObjectName + ", " + sourceMappingObjectName + "." + mappingAttr.getGetterName() + ");\n");
			}
			else if (assoc instanceof final OneToManyAssociation otm && otm.isBidirectional() && updateMode) {
				// We must call the respective method in order to import the data
				final var methodName = "import" + assoc.getUpperCaseName();
				final var getter = new StringBuilder();
				final EList<AbstractDomainAssociation> assocList = getDomainAttributeAssocs(mappingAttr, parentAssoc);

				// It might be the case that a one-to-many association is not directly accessible via the selected domain object!
				for (int i = assocList.size() - 1; i >= 0; i--) {
					final AbstractDomainAssociation ia = assocList.get(i);

					getter.append("." + ia.getGetterName());
				}

				// Also the mapping object could appear somewhere in the mapping hierarchy!
				if (parentMappingGetter != null && !parentMappingGetter.isEmpty())
					mappingGetter = "." + parentMappingGetter;

				b.append("\n// Import " + assoc.getTarget().getLabel() + " objects\n");

				// The import method should be invoked only if all parent associations are set properly!
				b.append(createDomainAssocIfStatement(mappingAttr, parentAssoc, targetObjectName, updateMode));

				b.append(methodName + "(" + targetObjectName + getter.toString());
				b.append(", " + sourceMappingObjectName + mappingGetter + ");\n");
			}
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @param targetObjectName
	 * @param assocToBeExcluded
	 * @param parentAssoc
	 * @return the generated content
	 */
	protected String addOneToOnePersistOperations(DataExchangeElement element, String targetObjectName,
			AbstractDomainAssociation assocToBeExcluded, AbstractDomainAssociation parentAssoc) {
		final var b = new StringBuilder();
		final var assocSet = new HashSet<AbstractDomainAssociation>();
		final var oneToOneAttrMap = new HashMap<ExchangeMappingAttribute, Integer>();

		for (final ExchangeMappingAttribute mappingAttr : element.getMappingObject().getAttributes()) {
			// If the attribute is not mapped to a domain attribute the association will be handled in a separate method!
			if (mappingAttr.getDomainAttribute() == null)
				continue;

			final AbstractDomainAssociation mappingAssoc = mappingAttr.getAssociation();

			if (mappingAssoc == null)
				continue;

			// If the calling method handles the (one-to-one) association it shouldn't be persisted at this point!
			if (mappingAssoc.equals(assocToBeExcluded))
				continue;

			if (mappingAssoc instanceof OneToOneAssociation && !assocSet.contains(mappingAssoc) && !mappingAssoc.isCascadePersist()) {
				oneToOneAttrMap.put(mappingAttr, mappingAttr.getAssociationList().size());
				assocSet.add(mappingAssoc);
			}
		}

		final List<Map.Entry<ExchangeMappingAttribute, Integer>> list = new LinkedList<>(oneToOneAttrMap.entrySet());
		list.sort((obj1, obj2) -> obj2.getValue().compareTo(obj1.getValue()));

		for (final Map.Entry<ExchangeMappingAttribute, Integer> entry : list) {
			final ExchangeMappingAttribute mappingAttr = entry.getKey();
			final AbstractDomainAssociation mappingAssoc = mappingAttr.getAssociation();
			final String otoObjName = mappingAssoc.getName();

			// We assume that we have to create and persist objects that are wired by a one-to-one association in any case!
			final RepositoryMethod persistMethod = mappingAssoc.getTarget()
					.findRepositoryMethod(RepositoryMethodTypeEnumeration.PERSIST);

			if (persistMethod == null)
				return createOutputForMissingRepositoryMethod(mappingAssoc.getTarget(), RepositoryMethodTypeEnumeration.PERSIST);

			final Repository repository = persistMethod.getRepository();
			final String repositoryName = createRepositoryName(repository.getDomainObject());

			repositories.put(repositoryName, repository);

			b.append("\n" + otoObjName + " = " + repositoryName + "." + persistMethod.getName());
			b.append("(" + otoObjName + (persistMethod.addUniqueCheck() ? ", true" : "") + ", false, false);\n");
			b.append(targetObjectName + "." + getOneToOneGetter(mappingAttr, parentAssoc));
			b.append(mappingAssoc.getSetterName() + "(" + otoObjName + ");\n");
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @param assoc
	 * @param addNewItems
	 * @param deleteAllItems
	 * @param updateExistingItems
	 * @param deleteThisItem
	 * @return the generated content
	 */
	protected String createImportForElement(DataExchangeElement element, AbstractDomainAssociation assoc, boolean addNewItems,
			boolean deleteAllItems, boolean updateExistingItems, boolean deleteThisItem) {
		final var b = new StringBuilder();
		RepositoryMethod persistMethod = null;
		DomainAttribute attr = null;
		final ExchangeMappingObject mappingObject = element.getMappingObject();
		final String targetObjectName = mappingObject.getDomainObject().getLowerCaseName();
		RepositoryMethod finderMethod = mappingObject.getDomainObject()
				.findRepositoryMethod(RepositoryMethodTypeEnumeration.FIND_BY_ID);
		boolean usesCustomFinderMethod = false;
		boolean targetDomainObjectDeclared = false;
		boolean addElseStatement = false;
		DataExchangeElement rootElement = method.getRootElement();

		if (!method.isProcessSingleObject() && method.getContentType() == ContentTypeEnumeration.XML)
			rootElement = method.getRootElement().getSubElements().get(0);

		if (element.equals(rootElement) && usesCustomFinderMethod())
			usesCustomFinderMethod = true;

		if (!deleteAllItems && (deleteThisItem || updateExistingItems)) {
			if (!usesCustomFinderMethod) {
				// In order to attach a given domain object we must find a suitable attribute!
				if (mappingObject.getPKAttribute() != null)
					attr = mappingObject.getPKAttribute().getDomainAttribute();

				if (attr == null) {
					if (mappingObject.getDisplayAttribute() != null)
						attr = mappingObject.getDisplayAttribute().getDomainAttribute();

					if (attr == null) {
						b.append("//-----------------------------------GENERATOR WARNING--------------------------------------\n");
						b.append("//\n");
						b.append("// Element \"" + element.getName() + "\" needs a primary key or a display attribute!\n");
						b.append("//\n");
						b.append("//------------------------------------------------------------------------------------------\n");

						return b.toString();
					}

					finderMethod = getFinderMethod(mappingObject);

					if (finderMethod == null)
						return createOutputForMissingRepositoryMethod(mappingObject.getDomainObject(),
								RepositoryMethodTypeEnumeration.FIND_BY_ID);
				}
			}
			else {
				finderMethod = mappingObject.getDomainObject().findRepositoryMethod(RepositoryMethodTypeEnumeration.SEARCH);

				if (finderMethod == null)
					return createOutputForMissingRepositoryMethod(mappingObject.getDomainObject(), RepositoryMethodTypeEnumeration.SEARCH);
			}
		}

		if (finderMethod == null)
			return createOutputForMissingRepositoryMethod(mappingObject.getDomainObject(), RepositoryMethodTypeEnumeration.FIND_BY_ID);

		final Repository repository = finderMethod.getRepository();
		final String repoName = createRepositoryName(repository.getDomainObject());
		repositories.put(repoName, repository);

		if (generator != null)
			generator.importPackage(mappingObject.getDomainObject().getNamespace().toString());

		if (!deleteAllItems && (deleteThisItem || updateExistingItems)) {
			targetDomainObjectDeclared = true;

			b.append(mappingObject.getDomainObject().getName() + " " + targetObjectName + " = ");

			if (!usesCustomFinderMethod) {
				String getter = null;
				ExchangeMappingAttribute finderAttr = null;

				// If the mapping object doesn't provide the primary key attribute it will contain the display attribute at least!
				if (mappingObject.getPKAttribute() != null) {
					finderAttr = mappingObject.getPKAttribute();
					getter = mappingObject.getPKAttribute().getGetterName();
				}
				else {
					finderAttr = mappingObject.getDisplayAttribute();
					getter = mappingObject.getDisplayAttribute().getGetterName();
				}

				b.append("null;\n\n");

				// Before searching for an existing object the respective mapping attribute must be checked if it contains appropriate
				// data! A field of type "int" or "long" assigned with a value of "0" indicates that the import interface provides an
				// object that doesn't exist in our database yet!
				b.append("if(" + DEFAULT_MAPPING_OBJ_NAME + "." + getter + " != ");
				b.append(attr.getJavaType().getLocalVariableDefaultValue());
				b.append(")\n");
				b.append(targetObjectName + " = ");
				b.append(
						createFinderFragment(finderAttr, mappingObject.getDomainObject(), DEFAULT_MAPPING_OBJ_NAME + "." + getter, false));
				b.append(";\n\n");
			}
			else
				b.append(FINDER_METHOD_PREFIX + mappingObject.getDomainObject().getName() + "(" + DEFAULT_MAPPING_OBJ_NAME + ");\n\n");

			if (deleteThisItem) {
				b.append("// Delete existing " + mappingObject.getDomainObject().getLabel() + "!\n");
				b.append("if(" + targetObjectName + " != null)\n");
				b.append(repoName + "." + REPO_METHOD_NAME_DELETE_ENTITY + "(" + targetObjectName + ");\n\n");
			}
			else {
				addElseStatement = true;

				b.append("if(" + targetObjectName + " != null)\n");
				b.append("{\n");
				b.append(importDataExchangeElement(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName, repoName, null, true, assoc));
				b.append("}\n");
			}
		}

		if (addNewItems) {
			persistMethod = mappingObject.getDomainObject().findRepositoryMethod(RepositoryMethodTypeEnumeration.PERSIST);

			if (persistMethod == null)
				return createOutputForMissingRepositoryMethod(mappingObject.getDomainObject(), RepositoryMethodTypeEnumeration.PERSIST);

			if (addElseStatement) {
				b.append("else\n");
				b.append("{\n");
			}

			if (!targetDomainObjectDeclared)
				b.append("var ");

			b.append(targetObjectName + " = new " + mappingObject.getDomainObject().getName() + "();\n");
			b.append(initializeOneToOneAssociations(targetObjectName, mappingObject, null, assoc));

			// In case of bidirectional one-to-many associations we must wire both objects!
			if (assoc instanceof final OneToManyAssociation otm && otm.isBidirectional()) {
				final ManyToOneAssociation mto = otm.getReverseAssociation();

				b.append(targetObjectName + "." + mto.getSetterName() + "(" + assoc.getDomainObject().getLowerCaseName() + ");\n");
			}

			b.append(importDataExchangeElement(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName, repoName, null, false, assoc));
			b.append(addOneToOnePersistOperations(element, targetObjectName, null, assoc));
			b.append("\n" + targetObjectName + " = " + repoName + "." + persistMethod.getName());
			b.append("(" + targetObjectName + (persistMethod.addUniqueCheck() ? ", true" : "") + ", false, false);\n");

			// One-to-many associations must be handled after performing the persist operation of the parent object
			b.append(addOneToManyInvocations(element, DEFAULT_MAPPING_OBJ_NAME, targetObjectName));

			if (addElseStatement)
				b.append("}\n");
		}

		return b.toString();
	}

	/**
	 * @param mappingAttr
	 * @param domainObject
	 * @param getter
	 * @param useGetReference
	 * @return the generated content
	 */
	private String createFinderFragment(final ExchangeMappingAttribute mappingAttr, final DomainObject domainObject,
			final String getter, final boolean useGetReference) {
		final var b = new StringBuilder();
		final ExchangeMappingAttribute clientAttr = mappingAttr.getExchangeMappingObject().getClientAttribute();
		final DomainObject clientObject = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT);
		final RepositoryMethod finderMethod = getFinderMethod(mappingAttr, domainObject);

		if (finderMethod == null)
			return null;

		final Repository repository = finderMethod.getRepository();
		final String repositoryName = createRepositoryName(repository.getDomainObject());

		repositories.put(repositoryName, repository);

		// The method getReference() must not be used in case of 'FIND_BY_UNIQUE_KEY' methods!
		if (useGetReference && finderMethod.getMethodType() != RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY) {
			if (generator != null)
				generator.importPackage(domainObject.getNamespace().toString());

			b.append(repositoryName + "." + REPO_METHOD_NAME_GET_REFERENCE + "(" + domainObject.getName() + ".class, " + getter + ")");
			return b.toString();
		}

		b.append(repositoryName + "." + finderMethod.getName() + "(" + getter);

		// Usually, a 'FIND_BY_UNIQUE_KEY' method has a second parameter if the respective domain object is mandated!
		if (clientAttr != null && finderMethod.getMethodType() == RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY
				&& finderMethod.getMethodParameters().size() > 1) {
			final String mappingObjectName = getter.substring(0, getter.indexOf('.') + 1);

			if (generator != null)
				generator.importPackage(clientObject.getNamespace().toString());

			b.append(", " + repositoryName + "." + REPO_METHOD_NAME_GET_REFERENCE);
			b.append("(" + clientObject.getName() + ".class, " + mappingObjectName + clientAttr.getGetterName() + ")");
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * Search finder method by using an exchange mapping attribute and a domain object
	 * @param mappingAttr
	 * @param domainObject
	 * @return a repository method of type 'FIND_BY_ID' or 'FIND_BY_UNIQUE_KEY'. It returns null if no appropriate method exists!
	 */
	private RepositoryMethod getFinderMethod(final ExchangeMappingAttribute mappingAttr, final DomainObject domainObject) {
		final DomainAttribute domainAttr = mappingAttr.getDomainAttribute();
		final ExchangeMappingAttribute clientAttr = mappingAttr.getExchangeMappingObject().getClientAttribute();

		if (domainAttr.equals(domainObject.getPKAttribute()))
			return domainObject.findRepositoryMethod(RepositoryMethodTypeEnumeration.FIND_BY_ID);

		if (domainAttr.equals(domainObject.getDisplayAttribute())) {
			final RepositoryMethod finderMethod = domainObject.findRepositoryMethod(RepositoryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY,
					domainAttr);

			if (finderMethod == null)
				return null;

			if (domainObject.isMandated() && clientAttr == null && finderMethod.getMethodParameters().size() > 1)
				return null;

			return finderMethod;
		}

		return null;
	}

	/**
	 * Search finder method by using an exchange mapping object
	 * @param mappingObject
	 * @return a repository method of type 'FIND_BY_ID' or 'FIND_BY_UNIQUE_KEY'. It returns null if no appropriate method exists!
	 */
	private RepositoryMethod getFinderMethod(final ExchangeMappingObject mappingObject) {
		RepositoryMethod finderMethod = null;

		if (mappingObject.getPKAttribute() != null)
			finderMethod = getFinderMethod(mappingObject.getPKAttribute(), mappingObject.getDomainObject());

		if (finderMethod != null)
			return finderMethod;

		if (mappingObject.getDisplayAttribute() != null)
			return getFinderMethod(mappingObject.getDisplayAttribute(), mappingObject.getDomainObject());

		return null;
	}

	/**
	 * Perform the conversion of the mapping type to the type of the domain attribute
	 * @param domainAttribute
	 * @return the generated content
	 */
	private String addTypeConversion(DomainAttribute domainAttribute) {
		if (!domainAttribute.getJavaType().isChar())
			return "";

		if (domainAttribute.getCollectionType() == CollectionTypeEnumeration.SET) {
			if (generator != null)
				generator.importPackage("java.util.stream");

			return ".stream().map(s -> s.charAt(0)).collect(Collectors.toSet())";
		}
		else if (domainAttribute.getCollectionType() == CollectionTypeEnumeration.LIST)
			return ".stream().map(s -> s.charAt(0)).toList()";

		return ".charAt(0)";
	}

}
