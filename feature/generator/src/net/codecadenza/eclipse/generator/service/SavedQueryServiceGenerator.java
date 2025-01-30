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

import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the saved query service interface
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SavedQueryServiceGenerator extends AbstractJavaSourceGenerator {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public SavedQueryServiceGenerator(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString() + PACK_SERVICE;

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVICE, SAVED_QUERY_SERVICE, packageName);
		javaFile.setComment("Service interface for saved queries");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importClass("net.codecadenza.runtime.search.dto.SearchDTO");
		importPackage("java.util");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public interface " + SAVED_QUERY_SERVICE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addFieldWithoutAccessModifier(JavaType.STRING, "LAST_QUERY_TITLE").withDefaultValue("\"LAST_QUERY\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final DomainObject taggedObject = project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY);
		final AbstractDomainAssociation ownerAssoc = getUserAssociation(taggedObject);
		final String ownerPKType = ownerAssoc.getTarget().getPKAttribute().getJavaType().getName();
		var methodSignature = "SearchDTO getLastQuery(" + ownerPKType + " ownerId, String viewName)";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Get last query of given view and user\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @return the search object\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + ";\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void saveQuery(" + ownerPKType + " ownerId, String viewName, String title, SearchDTO query)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Save query\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @param title\n");
		b.append(" * @param query\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + ";\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "SearchDTO getSavedQuery(" + ownerPKType + " ownerId, String viewName, String title)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Get saved query\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @param title\n");
		b.append(" * @return the search object\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + ";\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void deleteSavedQuery(" + ownerPKType + " ownerId, String viewName, String title)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Delete saved query\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @param title\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + ";\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "Collection<String> getSavedQueries(" + ownerPKType + " ownerId, String viewName)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Get all saved queries of given view and user\n");
		b.append(" * @param ownerId\n");
		b.append(" * @param viewName\n");
		b.append(" * @return a string collection of saved query titles\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append(methodSignature + ";\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Determine the user association
	 * @param taggedObject
	 * @return the association that represents the owner of the saved query
	 * @throws IllegalStateException if the respective user association could not be found
	 */
	public static AbstractDomainAssociation getUserAssociation(DomainObject taggedObject) {
		AbstractDomainAssociation ownerAssoc = null;

		if (taggedObject == null)
			throw new IllegalStateException("The parameter 'taggedObject' must not be null!");

		for (final AbstractDomainAssociation assoc : taggedObject.getAssociations())
			if (assoc.getTag() == AssociationTagEnumeration.SAVEDQUERY_OWNER) {
				ownerAssoc = assoc;

				final DomainObject userObject = ownerAssoc.getTarget();

				if (userObject.getTag() != DomainTagEnumeration.USER)
					throw new IllegalStateException("The domain object that represents the user is not tagged properly!");

				break;
			}

		if (ownerAssoc == null)
			throw new IllegalStateException("The association that represents the owner could not be found!");

		return ownerAssoc;
	}

}
