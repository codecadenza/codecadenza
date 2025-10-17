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
package net.codecadenza.eclipse.model.exchange;

import net.codecadenza.eclipse.model.service.ServiceMethod;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Data Exchange Method</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getExchangeMode <em>Exchange Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement <em>Root Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getContentType <em>Content Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getParser <em>Parser</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isPerformValidation <em>Perform Validation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCharset <em>Charset</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isFormatOutput <em>Format Output</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean <em>Data Exchange Service
 * Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isProcessSingleObject <em>Process Single Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getJoinedImportMethod <em>Joined Import Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getQuoteCharacter <em>Quote Character</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRecordSeparator <em>Record Separator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCommentCharacter <em>Comment Character</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDelimiter <em>Delimiter</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateFormat <em>Default Date Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateTimeFormat <em>Default Date Time
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultNumberFormat <em>Default Number
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getAssociationControllers <em>Association
 * Controllers</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSchemaFileName <em>Schema File Name</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod()
 * @model
 * @generated
 */
public interface DataExchangeMethod extends ServiceMethod {
	/**
	 * Return the value of the '<em><b>Data Exchange Service Bean</b></em>' container reference. It is bidirectional and its
	 * opposite is ' {@link net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods <em>Data Exchange
	 * Methods</em>}'.
	 * @return the value of the '<em>Data Exchange Service Bean</em>' container reference
	 * @see #setDataExchangeServiceBean(DataExchangeServiceBean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DataExchangeServiceBean()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods
	 * @model opposite="dataExchangeMethods" transient="false"
	 * @generated
	 */
	DataExchangeServiceBean getDataExchangeServiceBean();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean <em>Data
	 * Exchange Service Bean</em>}' container reference
	 * @param value the new value of the '<em>Data Exchange Service Bean</em>' container reference
	 * @see #getDataExchangeServiceBean()
	 * @generated
	 */
	void setDataExchangeServiceBean(DataExchangeServiceBean value);

	/**
	 * Return the value of the '<em><b>Process Single Object</b></em>' attribute
	 * @return the value of the '<em>Process Single Object</em>' attribute
	 * @see #setProcessSingleObject(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ProcessSingleObject()
	 * @model
	 * @generated
	 */
	boolean isProcessSingleObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isProcessSingleObject <em>Process
	 * Single Object</em>}' attribute
	 * @param value the new value of the '<em>Process Single Object</em>' attribute
	 * @see #isProcessSingleObject()
	 * @generated
	 */
	void setProcessSingleObject(boolean value);

	/**
	 * Return the value of the '<em><b>Joined Import Method</b></em>' reference
	 * @return the value of the '<em>Joined Import Method</em>' reference
	 * @see #setJoinedImportMethod(DataExchangeMethod)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_JoinedImportMethod()
	 * @model
	 * @generated
	 */
	DataExchangeMethod getJoinedImportMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getJoinedImportMethod <em>Joined
	 * Import Method</em>}' reference
	 * @param value the new value of the '<em>Joined Import Method</em>' reference
	 * @see #getJoinedImportMethod()
	 * @generated
	 */
	void setJoinedImportMethod(DataExchangeMethod value);

	/**
	 * Return the value of the '<em><b>Quote Character</b></em>' attribute
	 * @return the value of the '<em>Quote Character</em>' attribute
	 * @see #setQuoteCharacter(char)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_QuoteCharacter()
	 * @model
	 * @generated
	 */
	char getQuoteCharacter();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getQuoteCharacter <em>Quote
	 * Character</em>}' attribute
	 * @param value the new value of the '<em>Quote Character</em>' attribute
	 * @see #getQuoteCharacter()
	 * @generated
	 */
	void setQuoteCharacter(char value);

	/**
	 * Return the value of the '<em><b>Record Separator</b></em>' attribute
	 * @return the value of the '<em>Record Separator</em>' attribute
	 * @see #setRecordSeparator(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_RecordSeparator()
	 * @model
	 * @generated
	 */
	String getRecordSeparator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRecordSeparator <em>Record
	 * Separator</em>}' attribute
	 * @param value the new value of the '<em>Record Separator</em>' attribute
	 * @see #getRecordSeparator()
	 * @generated
	 */
	void setRecordSeparator(String value);

	/**
	 * Return the value of the '<em><b>Comment Character</b></em>' attribute
	 * @return the value of the '<em>Comment Character</em>' attribute
	 * @see #setCommentCharacter(char)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_CommentCharacter()
	 * @model
	 * @generated
	 */
	char getCommentCharacter();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCommentCharacter <em>Comment
	 * Character</em>}' attribute
	 * @param value the new value of the '<em>Comment Character</em>' attribute
	 * @see #getCommentCharacter()
	 * @generated
	 */
	void setCommentCharacter(char value);

	/**
	 * Return the value of the '<em><b>Delimiter</b></em>' attribute
	 * @return the value of the '<em>Delimiter</em>' attribute
	 * @see #setDelimiter(char)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Delimiter()
	 * @model
	 * @generated
	 */
	char getDelimiter();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDelimiter <em>Delimiter</em>}'
	 * attribute
	 * @param value the new value of the '<em>Delimiter</em>' attribute
	 * @see #getDelimiter()
	 * @generated
	 */
	void setDelimiter(char value);

	/**
	 * Return the value of the '<em><b>Default Date Format</b></em>' attribute
	 * @return the value of the '<em>Default Date Format</em>' attribute
	 * @see #setDefaultDateFormat(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultDateFormat()
	 * @model
	 * @generated
	 */
	String getDefaultDateFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateFormat <em>Default Date
	 * Format</em>}' attribute
	 * @param value the new value of the '<em>Default Date Format</em>' attribute
	 * @see #getDefaultDateFormat()
	 * @generated
	 */
	void setDefaultDateFormat(String value);

	/**
	 * Return the value of the '<em><b>Default Date Time Format</b></em>' attribute
	 * @return the value of the '<em>Default Date Time Format</em>' attribute
	 * @see #setDefaultDateTimeFormat(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultDateTimeFormat()
	 * @model
	 * @generated
	 */
	String getDefaultDateTimeFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateTimeFormat <em>Default
	 * Date Time Format</em>}' attribute
	 * @param value the new value of the '<em>Default Date Time Format</em>' attribute
	 * @see #getDefaultDateTimeFormat()
	 * @generated
	 */
	void setDefaultDateTimeFormat(String value);

	/**
	 * Return the value of the '<em><b>Default Number Format</b></em>' attribute
	 * @return the value of the '<em>Default Number Format</em>' attribute
	 * @see #setDefaultNumberFormat(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultNumberFormat()
	 * @model
	 * @generated
	 */
	String getDefaultNumberFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultNumberFormat <em>Default
	 * Number Format</em>}' attribute
	 * @param value the new value of the '<em>Default Number Format</em>' attribute
	 * @see #getDefaultNumberFormat()
	 * @generated
	 */
	void setDefaultNumberFormat(String value);

	/**
	 * Return the value of the '<em><b>Association Controllers</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.AssociationController}.
	 * @return the value of the '<em>Association Controllers</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_AssociationControllers()
	 * @model containment="true"
	 * @generated
	 */
	EList<AssociationController> getAssociationControllers();

	/**
	 * Return the value of the '<em><b>Schema File Name</b></em>' attribute
	 * @return the value of the '<em>Schema File Name</em>' attribute
	 * @see #setSchemaFileName(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_SchemaFileName()
	 * @model
	 * @generated
	 */
	String getSchemaFileName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSchemaFileName <em>Schema File
	 * Name</em>}' attribute
	 * @param value the new value of the '<em>Schema File Name</em>' attribute
	 * @see #getSchemaFileName()
	 * @generated
	 */
	void setSchemaFileName(String value);

	/**
	 * Return the value of the '<em><b>Exchange Mode</b></em>' containment reference
	 * @return the value of the '<em>Exchange Mode</em>' containment reference
	 * @see #setExchangeMode(DataExchangeMode)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ExchangeMode()
	 * @model containment="true"
	 * @generated
	 */
	DataExchangeMode getExchangeMode();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getExchangeMode <em>Exchange
	 * Mode</em>}' containment reference
	 * @param value the new value of the '<em>Exchange Mode</em>' containment reference
	 * @see #getExchangeMode()
	 * @generated
	 */
	void setExchangeMode(DataExchangeMode value);

	/**
	 * Return the value of the '<em><b>Root Element</b></em>' containment reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod <em>Data Exchange Method</em>}'.
	 * @return the value of the '<em>Root Element</em>' containment reference
	 * @see #setRootElement(DataExchangeElement)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_RootElement()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod
	 * @model opposite="dataExchangeMethod" containment="true"
	 * @generated
	 */
	DataExchangeElement getRootElement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement <em>Root Element</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Root Element</em>' containment reference
	 * @see #getRootElement()
	 * @generated
	 */
	void setRootElement(DataExchangeElement value);

	/**
	 * Return the value of the '<em><b>Content Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration}.
	 * @return the value of the '<em>Content Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration
	 * @see #setContentType(ContentTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ContentType()
	 * @model
	 * @generated
	 */
	ContentTypeEnumeration getContentType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getContentType <em>Content Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Content Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration
	 * @see #getContentType()
	 * @generated
	 */
	void setContentType(ContentTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Method Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration}.
	 * @return the value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration
	 * @see #setMethodType(DataExchangeMethodTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_MethodType()
	 * @model
	 * @generated
	 */
	DataExchangeMethodTypeEnumeration getMethodType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getMethodType <em>Method Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration
	 * @see #getMethodType()
	 * @generated
	 */
	void setMethodType(DataExchangeMethodTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Parser</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration}.
	 * @return the value of the '<em>Parser</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration
	 * @see #setParser(ParserImplementationEnumeration)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Parser()
	 * @model
	 * @generated
	 */
	ParserImplementationEnumeration getParser();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getParser <em>Parser</em>}' attribute
	 * @param value the new value of the '<em>Parser</em>' attribute
	 * @see net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration
	 * @see #getParser()
	 * @generated
	 */
	void setParser(ParserImplementationEnumeration value);

	/**
	 * Return the value of the '<em><b>Perform Validation</b></em>' attribute
	 * @return the value of the '<em>Perform Validation</em>' attribute
	 * @see #setPerformValidation(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_PerformValidation()
	 * @model
	 * @generated
	 */
	boolean isPerformValidation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isPerformValidation <em>Perform
	 * Validation</em>}' attribute
	 * @param value the new value of the '<em>Perform Validation</em>' attribute
	 * @see #isPerformValidation()
	 * @generated
	 */
	void setPerformValidation(boolean value);

	/**
	 * Return the value of the '<em><b>Charset</b></em>' attribute
	 * @return the value of the '<em>Charset</em>' attribute
	 * @see #setCharset(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Charset()
	 * @model
	 * @generated
	 */
	String getCharset();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCharset <em>Charset</em>}'
	 * attribute
	 * @param value the new value of the '<em>Charset</em>' attribute
	 * @see #getCharset()
	 * @generated
	 */
	void setCharset(String value);

	/**
	 * Return the value of the '<em><b>Format Output</b></em>' attribute
	 * @return the value of the '<em>Format Output</em>' attribute
	 * @see #setFormatOutput(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_FormatOutput()
	 * @model
	 * @generated
	 */
	boolean isFormatOutput();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isFormatOutput <em>Format
	 * Output</em>}' attribute
	 * @param value the new value of the '<em>Format Output</em>' attribute
	 * @see #isFormatOutput()
	 * @generated
	 */
	void setFormatOutput(boolean value);

	/**
	 * Get the root element of this method
	 * @param searchInJoinedMethod if true it returns the root element of the joined import method
	 * @return the respective root element
	 * @generated not
	 */
	DataExchangeElement getRootElement(boolean searchInJoinedMethod);

	/**
	 * @return true if a method provides a parameter containing the absolute path to the file to be imported
	 * @generated not
	 */
	boolean hasPathParameter();

	/**
	 * @return true if this method returns the absolute path of the generated export file
	 * @generated not
	 */
	boolean returnsPath();

	/**
	 * @return the filter method parameter that is responsible for selecting a specific object
	 * @generated not
	 */
	FilterMethodParameter getSingleObjectFilterParam();

	/**
	 * @return true if this method returns the generated export content
	 * @generated not
	 */
	boolean returnsContent();

	/**
	 * @return the default file extension based on the method's content type
	 * @generated not
	 */
	String getDefaultFileExtension();

	/**
	 * @return a java.nio.charset.StandarCharsets constant or a string representation of the given character set if it cannot be
	 *         converted
	 * @generated not
	 */
	String getStandardCharset();

}
