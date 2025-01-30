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
package net.codecadenza.eclipse.generator.exchange;

import java.util.HashMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator;
import net.codecadenza.eclipse.generator.exchange.method.imp.DataExchangeMethodGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;

/**
 * <p>
 * Generator for data exchange service beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataExchangeBeanGenerator extends AbstractJavaSourceGenerator {
	private final DataExchangeServiceBean exchangeBean;
	private final Project project;
	private final HashMap<String, Repository> repositories = new HashMap<>();
	private final HashMap<DataExchangeMethod, HashMap<String, Repository>> methods = new HashMap<>();
	private boolean hasImportMethod;

	/**
	 * Constructor
	 * @param exchangeBean
	 */
	public DataExchangeBeanGenerator(DataExchangeServiceBean exchangeBean) {
		super(exchangeBean.getSourceFile());

		this.exchangeBean = exchangeBean;
		this.project = exchangeBean.getNamespace().getProject();

		exchangeBean.getDataExchangeMethods().forEach(method -> {
			this.repositories.putAll(DataExchangeMethodGeneratorFactory.getMethodGenerator(this, method).getRepositories());
			this.methods.put(method, repositories);

			if (method.getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT)
				this.hasImportMethod = true;
		});

		// Remove all content that has been created during initialization!
		clearContent();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (project.isJakartaEEApplication()) {
			importPackage("jakarta.ejb");
			importPackage("jakarta.annotation.security");

			if (hasImportMethod)
				importPackage("jakarta.transaction");
		}
		else if (project.isSpringBootApplication()) {
			importClass("org.springframework.stereotype.Service");
			importPackage("jakarta.persistence");

			if (hasImportMethod) {
				importPackage("org.springframework.transaction.support");
				importPackage("org.springframework.transaction");
			}
		}
		else {
			importPackage("jakarta.persistence");
			importPackage("net.codecadenza.runtime.jpa");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isJakartaEEApplication()) {
			b.append("@Stateless\n");
			b.append("@TransactionManagement(TransactionManagementType.BEAN)\n");
		}
		else if (project.isSpringBootApplication())
			b.append("@Service\n");

		b.append("public class " + exchangeBean.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (project.isJakartaEEApplication()) {
			if (hasImportMethod)
				addPrivateField("UserTransaction", AbstractExchangeMethodGenerator.TRANSACTION_OBJ_NAME).create();
		}
		else if (project.isSpringBootApplication()) {
			addProtectedField("EntityManager", "em").withAnnotations("@PersistenceContext\n").create();

			if (hasImportMethod)
				addPrivateField("PlatformTransactionManager", "transactionManager").inject().create();
		}
		else
			addPrivateField("EntityManager", "em").withFinalModifier().create();

		repositories.keySet().forEach(repositoryName -> {
			final Repository repository = repositories.get(repositoryName);
			final String typeName;

			if (!project.isBoundaryMode()) {
				final BoundaryBean boundary = project.getBoundaryByDomainObject(repository.getDomainObject());

				typeName = boundary.getInterfaceName();
			}
			else
				typeName = repository.getName();

			if (project.isJakartaEEApplication() || project.isSpringBootApplication())
				addPrivateField(typeName, repositoryName).inject().create();
			else
				addPrivateField(typeName, repositoryName).withFinalModifier().create();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		if (project.isJavaSEApplication()) {
			var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + exchangeBean.getName() + "()\n");
			b.append("{\n");
			b.append("this.em = PersistenceEngine.getEntityManager();\n");

			for (final Map.Entry<String, Repository> entry : repositories.entrySet()) {
				final Repository repository = entry.getValue();

				if (!project.isBoundaryMode()) {
					final BoundaryBean boundary = project.getBoundaryByDomainObject(repository.getDomainObject());

					if (boundary != null)
						b.append("this." + entry.getKey() + " = new " + boundary.getInterfaceName() + "(em);\n");
				}
				else
					b.append("this." + entry.getKey() + " = new " + repository.getName() + "(em);\n");
			}

			b.append("}\n\n");

			addConstructor(exchangeBean.getName() + "()", b.toString());

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param em\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + exchangeBean.getName() + "(EntityManager em)\n");
			b.append("{\n");
			b.append("this.em = em;\n");

			for (final Map.Entry<String, Repository> entry : repositories.entrySet()) {
				final Repository repository = entry.getValue();

				if (!project.isBoundaryMode()) {
					final BoundaryBean boundary = project.getBoundaryByDomainObject(repository.getDomainObject());

					if (boundary != null)
						b.append("this." + entry.getKey() + " = new " + boundary.getInterfaceName() + "(em);\n");
				}
				else
					b.append("this." + entry.getKey() + " = new " + repository.getName() + "(em);\n");
			}

			b.append("}\n\n");

			addConstructor(exchangeBean.getName() + "(EntityManager em)", b.toString());
		}
		else
			super.addConstructors();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (project.isJakartaEEApplication() && hasImportMethod)
			addResourceSetterMethod("UserTransaction", AbstractExchangeMethodGenerator.TRANSACTION_OBJ_NAME);

		exchangeBean.getDataExchangeMethods().forEach(method -> {
			final AbstractExchangeMethodGenerator methodGenerator = DataExchangeMethodGeneratorFactory.getMethodGenerator(this, method);
			methodGenerator.setRepositories(methods.get(method));
			methodGenerator.addImports();
			methodGenerator.createMethod();
			methodGenerator.createAdditionalMethods();
		});

		if (project.isJavaSEApplication()) {
			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Close entity manager\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void closeEntityManager()\n");
			b.append("{\n");
			b.append("if(em != null)\n");
			b.append("PersistenceEngine.releaseEntityManager(em);\n");
			b.append("}\n\n");

			addMethod("void closeEntityManager()", b.toString());
		}
	}

}
