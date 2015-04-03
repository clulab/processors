package edu.arizona.sista.odin.export.biopax

import java.io._

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

import org.biopax.paxtools.io._
import org.biopax.paxtools.model._
import org.biopax.paxtools.model.level3._

/**
  * Defines classes and methods used to build and output BioPax models.
  *   Written by Tom Hicks. 3/6/2015.
  *   Last Modified: Refactor some code to new mention manager.
  */
class BioPaxer {
  // Type aliases:
  type Gene = Protein                       // temporary until we decide how to handle genes
  type GeneReference = ProteinReference     // temporary until we decide how to handle genes

  // Constants:
  // val DegradationInteraction = "organic substance degradation"
  val DirectInteraction = "direct interaction"
  val MapsToPhysicalEntity = Set("Gene_or_gene_product", "Protein",
                                 "Protein_with_site", "Simple_chemical")
  val SistaBaseURL = "http://nlp.sista.arizona.edu/odin/"
  val SistaDefaultCharset = "UTF-8"

  // external knowledge base accessors
  protected val cellCompKB = new GeneOntologyKBAccessor
  protected val geneKB = new GeneOntologyKBAccessor
  protected val moleInterKB = new MolecularInteractionsKBAccessor
  protected val proteinKB = new UniprotKBAccessor
  protected val smallMoleculeKB = new ChEBIKBAccessor

  // incrementing ID for numbering entities
  protected val idCntr = new IncrementingId()

  // local caches for entities: map entity type names to entity reference classes
  protected val geneRefs = scala.collection.mutable.Map[String, GeneReference]()
  protected val proteinRefs = scala.collection.mutable.Map[String, ProteinReference]()
  protected val smallMoleculeRefs = scala.collection.mutable.Map[String, SmallMoleculeReference]()
  protected val vocabRefs = scala.collection.mutable.Map[String, ControlledVocabulary]()


  //
  // Public API:
  //

  /** Build and return a BioPax model for the given sequence of mentions. */
  def buildModel (mentions:Seq[Mention], doc:Document): Model = {
    // create and initialize a new BioPAX model:
    val factory: BioPAXFactory = BioPAXLevel.L3.getDefaultFactory()
    var model:Model = factory.createModel()
    // model.setXmlBase(SistaBaseURL)          // TODO: UNCOMMENT LATER?

    // TODO: Set XML namespace for SISTA NLP UAZ?
    // TODO: Add dataSource (Provenance) information for SISTA NLP UAZ

    addPublicationXref(model, doc)          // add publication xref for the current document

    initializeModel(model)

    // use only mentions labeled as Events for the roots of the forest trees:
    mentions.filter(_.matches("Event")).foreach {
      doEvents(model, _)
    }

    return model
  }


  /** return the given model as a single BioPax OWL string. */
  def modelToString (model:Model): String = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    val baos:ByteArrayOutputStream = new ByteArrayOutputStream()
    bpIOH.convertToOWL(model, baos)
    return baos.toString(SistaDefaultCharset)
  }

  /** Output the given model to the given output stream. */
  def outputModel (model:Model, out:OutputStream): Unit = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    bpIOH.convertToOWL(model, out)
  }


  //
  // Private Methods
  //

  /** Add the given mention to the given model and return the model. */
  private def doEvents (model:Model, mention: Mention): Model = {
    // TODO: parse and accumulate arguments in structure
    mention.label match {                   // dispatch on mention type
      case "Binding" => return doBinding(model, mention)
      case "Degradation" => return doDegradation(model, mention)
      case "Exchange" =>
      case "Expression" =>
      case "Hydrolysis" =>
      case "Hydroxylation" =>
      case "Negative_regulation" =>
      case "Phosphorylation" => return doPhosphorylation(model, mention)
      case "Positive_regulation" =>
      case "Regulation" =>
      case "Site" =>
      case "Translation" =>
      case "Transcription" =>
      case "Transport" =>
      case "Ubiquitination" =>
      case _ => ()
    }
    return model                            // return updated model
  }

  /** Augment model with a representation of Binding action. */
  private def doBinding (model:Model, mention:Mention): Model = {
    val themes = getTheme(mention)
    if (themes != null) {
      val reactants = mentionsToPhysicalEntities(model, themes)
      if (!reactants.isEmpty) {
        val complex:Complex = addComplex(model, reactants)
        if (complex != null) {
          val cplxAss:ComplexAssembly = addComplexAssembly(model, reactants, complex)
        }
      }
    }
    return model
  }

  /** Augment model with a representation of Degradation action. */
  private def doDegradation (model:Model, mention:Mention): Model = {
    val themes = getTheme(mention)
    if (themes != null) {
      val substrates = mentionsToPhysicalEntities(model, themes)
      if (!substrates.isEmpty) {
        val substrate = substrates(0)       // if more than one ignore rest
        val eUrl = genInternalURL(s"Degradation_${idCntr.genNextId()}")
        val deg:Degradation = model.addNew(classOf[Degradation], eUrl)
        deg.addLeft(substrate)              // substrate is left, right not allowed
        // deg.addInteractionType(vocabRefs(DegradationInteraction).asInstanceOf[InteractionVocabulary])
        deg.addComment(mention.text)
        deg.setDisplayName(s"Degradation of ${substrate.getDisplayName()}")
      }
    }
    return model
  }

  private def doPhosphorylation (model:Model, mention:Mention): Model = {
    // val themes = getTheme(mention)
    // if (themes != null) {
    //   val theme = themes(0)                 // should be only one theme
    //   val left = addTextBoundMention(model, theme)
    //   val right = addTextBoundMention(model, theme)
    // }
    return null
  }


  /** Add a binding feature to the given entity and the model, then return it. */
  private def addBindingFeature (model:Model, entity:PhysicalEntity): BindingFeature = {
    val eUrl = genInternalURL(s"BindingFeature_${idCntr.genNextId()}")
    val bf:BindingFeature = model.addNew(classOf[BindingFeature], eUrl)
    entity.addFeature(bf)
    return bf
  }

  /** Create a complex instance out of the given sequence of physical entities,
    * add it to the model, and return it. */
  private def addComplex (model:Model, lefts:Seq[PhysicalEntity]): Complex = {
    if (lefts.isEmpty) return null          // sanity check
    if (lefts.length != 2) return null      // NB: only do duplex complexes, for now
    val cUrl = genInternalURL(s"CPLX_${idCntr.genNextId()}")
    val complex:Complex = model.addNew(classOf[Complex], cUrl)
    complex.setDisplayName(lefts.map(_.getDisplayName()).mkString("+"))
    lefts.foreach { complex.addComponent(_) }
    val bf0:BindingFeature = addBindingFeature(model, lefts(0))
    val bf1:BindingFeature = addBindingFeature(model, lefts(1))
    bf0.setBindsTo(bf1)
    bf1.setBindsTo(bf0)
    return complex
  }

  /** Create a complex assembly instance from the given reactants and product,
    * add it to the model, and return it. */
  private def addComplexAssembly (model:Model,
                                  reactants:Seq[PhysicalEntity],
                                  product:Complex): ComplexAssembly =
  {
    val caName = s"ComplexAssembly_${idCntr.genNextId()}"
    val caUrl = genInternalURL(caName)
    val cplxAss:ComplexAssembly = model.addNew(classOf[ComplexAssembly], caUrl)
    cplxAss.setDisplayName(caName)
    cplxAss.setConversionDirection(ConversionDirectionType.LEFT_TO_RIGHT)
    reactants.foreach { cplxAss.addLeft(_) }  // set the reactants on the left
    cplxAss.addRight(product)                 // set the product on the right
    cplxAss.addInteractionType(vocabRefs(DirectInteraction).asInstanceOf[InteractionVocabulary])
    return cplxAss
  }


  /** Add a gene instance and associated entity reference to the model. */
  private def addGene (model:Model, mention:Mention, gRef:GeneReference): Gene = {
    val name = geneKB.getLookupKey(mention)
    val gUrl = genInternalURL(s"GOGP_${idCntr.genNextId()}")
    val gene:Gene = model.addNew(classOf[Gene], gUrl)
    gene.setDisplayName(name)
    gene.setEntityReference(gRef)
    return gene
  }

  /** Add a protein instance and associated entity reference to the model. */
  private def addProtein (model:Model, mention:Mention, pRef:ProteinReference): Protein = {
    val name = proteinKB.getLookupKey(mention)
    val pUrl = genInternalURL(s"PROT_${idCntr.genNextId()}")
    val prot:Protein = model.addNew(classOf[Protein], pUrl)
    prot.setDisplayName(name)
    prot.setEntityReference(pRef)
    return prot
  }

  /** Add a small molecule instance and associated entity reference to the model. */
  private def addSmallMolecule (model:Model, mention:Mention,
                                eRef:SmallMoleculeReference): SmallMolecule = {
    val name = smallMoleculeKB.getLookupKey(mention)
    val eUrl = genInternalURL(s"SM_${idCntr.genNextId()}")
    val sMole:SmallMolecule = model.addNew(classOf[SmallMolecule], eUrl)
    sMole.setDisplayName(name)
    sMole.setEntityReference(eRef)
    return sMole
  }

  /** Add a PublicationXref for the given document to the model. */
  private def addPublicationXref (model:Model, doc:Document) = {
    val pxrUrl = s"${SistaBaseURL}PX_${idCntr.genNextId()}"
    model.addNew(classOf[PublicationXref], pxrUrl)
  }

  /** Add a fixed controlled-vocabulary item to the model and the vocabulary references map. */
  private def addInteractionVocabularyConstant (
    model: Model,
    eNamespace: String,
    eName: String,
    eId: String,
    eDefinition: String
  ): ControlledVocabulary = {
    val eUrl = s"${SistaBaseURL}InteractionVocabulary_${idCntr.genNextId()}"
    val eRef:InteractionVocabulary = model.addNew(classOf[InteractionVocabulary], eUrl)
    eRef.addTerm(eName)
    if (!eDefinition.isEmpty)
      eRef.addComment(eDefinition)

    val uXref = genUnificationXref(model, eId, eNamespace)
    if (!eDefinition.isEmpty)
      uXref.addComment(eDefinition)
    eRef.addXref(uXref)

    vocabRefs.put(eName, eRef)              // memoize new reference
    return eRef                             // and return it
  }

  /** Add a fixed controlled-vocabulary item to the model and the vocabulary references map. */
  private def addVocabularyConstant[T <: ControlledVocabulary] (
    model: Model,
    vocabType: Class[T],
    extKB: ExternalKBAccessor,
    eName: String,
    eId: String,
    eDefinition: String
  ): T = {
    val eUrl = extKB.referenceURI(eId)
    val eRef:T = model.addNew(vocabType, eUrl)
    eRef.addTerm(eName)
    if (!eDefinition.isEmpty)
      eRef.addComment(eDefinition)

    val uXref = genUnificationXref(model, eId, extKB.namespace)
    if (!eDefinition.isEmpty)
      uXref.addComment(eDefinition)
    eRef.addXref(uXref)

    vocabRefs.put(eName, eRef)              // memoize new reference
    return eRef                             // and return it
  }


  /** Create gene instance and entity reference, add them to the model, and return instance. */
  private def doGene (model:Model, mention:Mention): Gene = {
    val gRef:GeneReference = referenceForGene(model, mention)
    return addGene(model, mention, gRef)
  }

  /** Create protein instance and entity reference, add them to the model, and return instance. */
  private def doProtein (model:Model, mention:Mention): Protein = {
    val pRef:ProteinReference = referenceForProtein(model, mention)
    return addProtein(model, mention, pRef)
  }

  /** Create protein instance and entity reference, add them to the model, and return instance. */
  private def doProteinWithSite (model:Model, mention:Mention): Protein = {
    val protMention = getProtein(mention)(0)  // extract protein mention, ignore site mention
    val pRef:ProteinReference = referenceForProtein(model, protMention)
    return addProtein(model, mention, pRef)
  }

  /** Create small molecule instance and entity reference, add them to the model, and return instance. */
  private def doSmallMolecule (model:Model, mention:Mention): SmallMolecule = {
    val smRef:SmallMoleculeReference = referenceForSmallMolecule(model, mention)
    return addSmallMolecule(model, mention, smRef)
  }

  /** Generate an internal URL for the given ID string. */
  private def genInternalURL (id:String): String = {
    return s"${SistaBaseURL}${id}"
  }

  /** Generate a unification Xref with the given arguments. */
  private def genUnificationXref (model:Model, eId:String, eNamespace:String): UnificationXref = {
    val xUrl = genInternalURL(s"UX_${idCntr.genNextId()}")
    val uXref:UnificationXref = model.addNew(classOf[UnificationXref], xUrl)
    uXref.setId(eId)
    uXref.setDb(eNamespace)
    return uXref
  }

  /** Return the named argument (a mention) from the arguments of the given mention. */
  private def getMentionArg (mention:Mention, argName:String): Seq[Mention] = {
    mention.arguments.foreach { case (k, vs) => if (k startsWith argName) return vs }
    return Nil
  }
  private def getControlled  (mention:Mention): Seq[Mention] = getMentionArg(mention, "controlled")
  private def getController  (mention:Mention): Seq[Mention] = getMentionArg(mention, "controller")
  private def getDestination (mention:Mention): Seq[Mention] = getMentionArg(mention, "destination")
  private def getGoal        (mention:Mention): Seq[Mention] = getMentionArg(mention, "goal")
  private def getProtein     (mention:Mention): Seq[Mention] = getMentionArg(mention, "protein")
  private def getTheme       (mention:Mention): Seq[Mention] = getMentionArg(mention, "theme")
  private def getSite        (mention:Mention): Seq[Mention] = getMentionArg(mention, "site")
  private def getSource      (mention:Mention): Seq[Mention] = getMentionArg(mention, "source")


  private def initializeModel (model:Model) = {
    // protein phosphorylation GO:0006468 (GeneOntology)
    // protein modification characterized by amino acid modified MOD:01157 (PSI-MOD)

    // direct interaction MI:0407 (PSI-MOD)
//    addVocabularyConstant(model, classOf[InteractionVocabulary], moleInterKB, DirectInteraction,
//      "MI:0407", "Interaction between molecules that are in direct contact with each other.")
    // addInteractionVocabularyConstant(model, "go", DegradationInteraction, "GO:1901575",
    //  "The chemical reactions and pathways resulting in the breakdown of an organic substance, any molecular entity containing carbon.")
    addInteractionVocabularyConstant(model, "psimi", DirectInteraction, "MI:0407",
      "Interaction between molecules that are in direct contact with each other.")
  }


  /** Return a vocabulary item for the cellular component represented by the given mention. */
  private def makeCellularComponent (model:Model, mention:Mention): ControlledVocabulary = {
    // cellular location is a property not an entity: must be attached to an entity (in future)
    return referenceForCellularComponent(model, mention)
  }

  /** Convert qualifying mentions in the given sequence to a sequence of physical entities.*/
  private def mentionsToPhysicalEntities (model:Model, mentions:Seq[Mention]): Seq[PhysicalEntity] = {
    return mentions.filter(m => MapsToPhysicalEntity(m.label)).map(toPhysicalEntity(model, _))
  }

  /** Lookup or create a controlled vocabulary item for the given cellular component mention. */
  private def referenceForCellularComponent (model:Model, mention:Mention): ControlledVocabulary = {
    val name = cellCompKB.getLookupKey(mention)
    return vocabRefs.getOrElseUpdate(name, registerCellularLocation(model, mention))
  }

  /** Lookup or create a reference for the given gene mention. */
  private def referenceForGene (model:Model, mention:Mention): GeneReference = {
    val name = geneKB.getLookupKey(mention)
    return geneRefs.getOrElseUpdate(name, registerGene(model, mention))
  }

  /** Lookup or create a reference for the given protein mention. */
  private def referenceForProtein (model:Model, mention:Mention): ProteinReference = {
    val name = proteinKB.getLookupKey(mention)
    return proteinRefs.getOrElseUpdate(name, registerProtein(model, mention))
  }

  /** Lookup or create a reference for the given small molecule mention. */
  private def referenceForSmallMolecule (model:Model, mention:Mention): SmallMoleculeReference = {
    val name = smallMoleculeKB.getLookupKey(mention)
    return smallMoleculeRefs.getOrElseUpdate(name, registerSmallMolecule(model, mention))
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerCellularLocation (model:Model, mention:Mention): CellularLocationVocabulary = {
    // TODO: error handling for the lookup:
    val eInfo = cellCompKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"CLV_GO:${idCntr.genNextId()}"  // MOCK missing referenceID value
    val eUrl = cellCompKB.referenceURI(eId)  // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:CellularLocationVocabulary = model.addNew(classOf[CellularLocationVocabulary], eUrl)
    eRef.addTerm(eName)
    eRef.addXref(uXref)

    vocabRefs.put(eName, eRef)              // memoize new reference
    return eRef                             // and return it
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerGene (model:Model, mention:Mention): GeneReference = {
    // TODO: error handling for the lookup:
    val eInfo = geneKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = idCntr.genNextIdWithFormat("GO:%07d")  // MOCK missing referenceID value
    val eUrl = geneKB.referenceURI(eId)     // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:GeneReference = model.addNew(classOf[GeneReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    geneRefs.put(eName, eRef)               // memoize new reference
    return eRef                             // and return it
  }


  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerProtein (model:Model, mention:Mention): ProteinReference = {
    // TODO: error handling for the lookup:
    val eInfo = proteinKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = idCntr.genNextIdWithFormat("P%05d") // MOCK missing referenceID value
    val eUrl = proteinKB.referenceURI(eId)  // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:ProteinReference = model.addNew(classOf[ProteinReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    proteinRefs.put(eName, eRef)            // memoize new reference
    return eRef                             // and return it
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerSmallMolecule (model:Model, mention:Mention): SmallMoleculeReference = {
    // TODO: error handling for the lookup:
    val eInfo = smallMoleculeKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"rSM_${idCntr.genNextId()}"  // MOCK missing referenceID value
    val eUrl = smallMoleculeKB.referenceURI(eId) // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:SmallMoleculeReference = model.addNew(classOf[SmallMoleculeReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    smallMoleculeRefs.put(eName, eRef)      // memoize new reference
    return eRef                             // and return it
  }


  /** Create a physical entity from the given mention, add it to the model, and return it. */
  private def toPhysicalEntity (model:Model, mention:Mention): PhysicalEntity = {
    mention.label match {
      case "Gene_or_gene_product" => return doGene(model, mention)
      case "Protein" => return doProtein(model, mention)
      case "Protein_with_site" => return doProteinWithSite(model, mention)
      case "Simple_chemical" => return doSmallMolecule(model, mention)
      case _ => return null
    }
  }

}


/** Implements an incrementing identification string for numbering entities. */
class IncrementingId {
  protected var cntr = 0

  /** Return the current identification string. */
  def currentId (): String = { s"${cntr}" }

  /** Increment counter and return new identification string. */
  def genNextId (): String = {
    cntr = cntr + 1
    return currentId()
  }

  /** Increment counter and return new identification string. */
  def genNextIdWithFormat (formatString:String): String = {
    cntr = cntr + 1
    return formatString.format(cntr)
  }
}
