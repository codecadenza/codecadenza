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
package net.codecadenza.eclipse.generator.service;

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_LANG;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_DELETE_ENTITY;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SEARCH;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;

import java.util.EnumMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the implementation of the saved query service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SavedQueryServiceBeanGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final DomainObject taggedObject;
	private final String serviceInterfacePack;
	private final Map<AttributeTagEnumeration, DomainAttribute> tagMapping = new EnumMap<>(AttributeTagEnumeration.class);
	private final AbstractDomainAssociation ownerAssoc;
	private final JavaType ownerPKType;
	private final String ownerPKNamespace;

	/**
	 * Constructor
	 * @param project
	 */
	public SavedQueryServiceBeanGenerator(Project project) {
		this.project = project;
		this.taggedObject = project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY);
		this.serviceInterfacePack = project.getRootNamespace().toString() + PACK_SERVICE;

		// Add attribute tags to the respective map
		for (final DomainAttribute att : taggedObject.getAttributes())
			if (att.getTag() != AttributeTagEnumeration.NONE)
				this.tagMapping.put(att.getTag(), att);

		this.ownerAssoc = SavedQueryServiceGenerator.getUserAssociation(taggedObject);
		this.ownerPKType = ownerAssoc.getTarget().getPKAttribute().getJavaType();

		if (ownerPKType.getNamespace() != null)
			this.ownerPKNamespace = ownerPKType.getNamespace().toString() + ".";
		else if (!ownerPKType.isPrimitive())
			this.ownerPKNamespace = PACK_JAVA_LANG + ".";
		else
			this.ownerPKNamespace = "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString() + PACK_SERVICE + SUB_PACKAGE_BEAN;

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVICE, SAVED_QUERY_SERVICE_BEAN, packageName);
		javaFile.setComment("Implementation of service for saved queries");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		final String ownerPackageName = ownerAssoc.getTarget().getNamespace().toString();

		importPackage("net.codecadenza.runtime.jpa");
		importClass("net.codecadenza.runtime.search.dto.SearchDTO");
		importPackage("java.io");
		importPackage("java.util");
		importPackage(taggedObject.getNamespace().toString());
		importPackage(ownerPackageName);
		importPackage(serviceInterfacePack);

		if (project.isSpringBootApplication()) {
			importPackage("org.springframework.transaction.annotation");
			importClass("org.springframework.stereotype.Repository");
		}
		else
			importPackage("jakarta.ejb");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		final JavaType pkType = taggedObject.getPKAttribute().getJavaType();

		if (project.isJakartaEEApplication()) {
			b.append("@Stateless\n");
			b.append("@Local(" + SAVED_QUERY_SERVICE + ".class)\n");
		}
		else
			b.append("@Repository\n");

		b.append("public class " + SAVED_QUERY_SERVICE_BEAN + " extends AbstractRepository");
		b.append("<" + taggedObject.getName() + ", ");
		b.append(pkType.getWrapperTypeName());
		b.append("> implements " + SAVED_QUERY_SERVICE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = "String buildQueryStatement(" + ownerPKType.getName()
				+ " ownerId, String viewName, String title, boolean allQueries)";

		b.append("/**\n");
		b.append(" * Build query statement\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @param title\n");
		b.append(" * @param allQueries\n");
		b.append(" * @return the query statement\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(title == null || title.isEmpty())\n");
		b.append("title = LAST_QUERY_TITLE;\n\n");
		b.append("String statement = \"select a from " + taggedObject.getName() + " a where a." + ownerAssoc.getName());
		b.append("." + ownerAssoc.getTarget().getPKAttribute().getName() + " = ");

		if (ownerPKType.isString() || ownerPKType.isUUID())
			b.append("'");

		b.append("\" + ownerId");

		if (ownerPKType.isString() || ownerPKType.isUUID())
			b.append(" + \"'\"");

		b.append(";\n");
		b.append("statement += \" and a." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_VIEW_NAME).getName());
		b.append(" = '\" + viewName + \"'");
		b.append(" and a." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_TITLE).getName() + " \";\n\n");
		b.append("if(allQueries)\n");
		b.append("statement += \"=\";\n");
		b.append("else\n");
		b.append("statement += \"!=\";\n\n");
		b.append("return statement += \"'\" + title + \"'\";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "SearchDTO getLastQuery(" + ownerPKType.getName() + " ownerId, String viewName)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + serviceInterfacePack + "." + SAVED_QUERY_SERVICE + "#getLastQuery(");
		b.append(ownerPKNamespace + ownerPKType.getName() + ", java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return getSavedQuery(ownerId, viewName, null);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void saveQuery(" + ownerPKType.getName() + " ownerId, String viewName, String title, SearchDTO query)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + serviceInterfacePack + "." + SAVED_QUERY_SERVICE);
		b.append("#saveQuery(" + ownerPKNamespace + ownerPKType.getName());
		b.append(", java.lang.String, java.lang.String, net.codecadenza.runtime.search.dto.SearchDTO)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());

		if (project.isSpringBootApplication())
			b.append("@Transactional\n");

		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("var ba = new ByteArrayOutputStream();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("new ObjectOutputStream(ba).writeObject(query);\n");
		b.append("}\n");
		b.append("catch (final IOException e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n\n");
		b.append("final Collection<" + taggedObject.getName() + "> savedQueries = " + REPO_METHOD_NAME_SEARCH);
		b.append("(buildQueryStatement(ownerId, viewName, title, true));\n\n");
		b.append("if(!savedQueries.isEmpty())\n");
		b.append("{\n");
		b.append("if(title == null || title.isEmpty())\n");

		addDebugLog(b, "Overwrite last query of view '{}' (owner id: '{}')", "viewName", "ownerId");

		b.append("else\n");

		addDebugLog(b, "Overwrite saved query '{}' of view '{}' (owner id: '{}')", "title", "viewName", "ownerId");

		b.append("\n");
		b.append("// Overwrite existing query\n");
		b.append("savedQueries.forEach(q -> q." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_DATA_OBJ).getSetterName());
		b.append("(ba.toByteArray()));\n\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(title == null || title.isEmpty())\n");
		b.append("{\n");

		addDebugLog(b, "Save last query of view '{}' (owner id: '{}')", "viewName", "ownerId");

		b.append("\n");
		b.append("title = LAST_QUERY_TITLE;\n");
		b.append("}\n");
		b.append("else\n");

		addDebugLog(b, "Save query '{}' of view '{}' (owner id: '{}')", "title", "viewName", "ownerId");

		b.append("\n");
		b.append("// Create new entry\n");
		b.append("final var savedQuery = new " + taggedObject.getName() + "();\n");
		b.append("savedQuery." + ownerAssoc.getSetterName() + "(" + REPO_METHOD_NAME_FIND_BY_ID);
		b.append("(" + ownerAssoc.getTarget().getName() + ".class, ownerId));\n");
		b.append("savedQuery." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_DATA_OBJ).getSetterName());
		b.append("(ba.toByteArray());\n");
		b.append("savedQuery." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_TITLE).getSetterName() + "(title);\n");
		b.append("savedQuery." + tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_VIEW_NAME).getSetterName() + "(viewName);\n\n");
		b.append(REPO_METHOD_NAME_PERSIST + "(savedQuery);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "SearchDTO getSavedQuery(" + ownerPKType.getName() + " ownerId, String viewName, String title)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + serviceInterfacePack + "." + SAVED_QUERY_SERVICE + "#");
		b.append("getSavedQuery(" + ownerPKNamespace + ownerPKType.getName() + ", java.lang.String, java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(title == null || title.isEmpty())\n");

		addDebugLog(b, "Get last query of view '{}' (owner id: '{}')", "viewName", "ownerId");

		b.append("else\n");

		addDebugLog(b, "Get query '{}' of view '{}' (owner id: '{}')", "title", "viewName", "ownerId");

		b.append("\n");

		b.append("final Collection<" + taggedObject.getName() + "> savedQueries = " + REPO_METHOD_NAME_SEARCH);
		b.append("(buildQueryStatement(ownerId, viewName, title, true));\n\n");
		b.append("for(final " + taggedObject.getName() + " query : savedQueries)\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var ois = new ObjectInputStream(new ByteArrayInputStream(query.");
		b.append(tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_DATA_OBJ).getGetterName() + "));\n");
		b.append("return (SearchDTO) ois.readObject();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n\n");
		b.append("return null;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void deleteSavedQuery(" + ownerPKType.getName() + " ownerId, String viewName, String title)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + serviceInterfacePack + "." + SAVED_QUERY_SERVICE + "#deleteSavedQuery(");
		b.append(ownerPKNamespace + ownerPKType.getName() + ", java.lang.String, java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");

		if (project.isSpringBootApplication())
			b.append("@Transactional\n");

		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Delete saved query '{}' of view '{}' (owner id: '{}')", "title", "viewName", "ownerId");

		b.append("\n");
		b.append(REPO_METHOD_NAME_SEARCH + "(buildQueryStatement(ownerId, viewName, title, true)).forEach(this::");
		b.append(REPO_METHOD_NAME_DELETE_ENTITY + ");\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "Collection<String> getSavedQueries(" + ownerPKType.getName() + " ownerId, String viewName)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + serviceInterfacePack + "." + SAVED_QUERY_SERVICE + "#getSavedQueries(");
		b.append(ownerPKNamespace + ownerPKType.getName() + ", java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Get saved queries of view '{}' (owner id: '{}')", "viewName", "ownerId");

		b.append("\n");
		b.append("return " + REPO_METHOD_NAME_SEARCH);
		b.append("(buildQueryStatement(ownerId, viewName, null, false)).stream().map(" + taggedObject.getName());
		b.append(tagMapping.get(AttributeTagEnumeration.SAVEDQUERY_TITLE).getGetterReference());
		b.append(").toList();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
